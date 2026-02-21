#!/usr/bin/env python3
"""Classify advice row-by-row for GPT template data with fixed input/output paths."""

import json
import os
import time
from typing import Any, Dict, List

import pandas as pd
from openai import OpenAI

INPUT_PATH = "data/gpt_template.csv"
OUTPUT_PATH = "data/df_advice_gpt5.csv"
MODEL_NAME = "gpt-5.1"
SLEEP_SECONDS = 0.0
TEMPERATURE = 0.0

CATEGORIES: List[str] = [
    "mix_and_match",
    "mismatch",
    "color_based",
    "shape_based",
    "spread",
    "mix_nutrients",
    "pure_nutrients",
    "spatial",
    "useall",
    "time",
    "blue_prioritize",
    "yellow_prioritize",
    "red_prioritize",
    "less_per_plant",
    "more_per_plant",
    "variation",
    "other",
    "noinfo",
]

SYSTEM_INSTRUCTIONS = """You are a data classifier. You will read ONE participant's advice line from an experiment and output a JSON object with binary flags (0/1) for each category.

Experiment context:
- Participants place nutrients (colors: blue, red, yellow) onto plants (different shapes) in a virtual field.
- In "high_appeal" treatment, plants appear in colors like blue, red, yellow, orange, purple, green. In "low_appeal" treatment, plants are gray.

IMPORTANT epistemic constraint:
- Participants do NOT know nutrient costs, do NOT know the true payoff function, and do NOT know the optimal strategy. 
- Therefore, do NOT infer cost-optimization or “correctness.” Classify ONLY based on what the advice text explicitly suggests.

Set category=1 if the advice includes it, else 0.

mix_and_match: ONLY valid for high_appeal treatment. Recommends matching nutrient colors to primary plant colors (blue, yellow, red) when possible and combining nutrients to recreate remaining secondary colors (orange, purple, green) based on color theory.
This category is a special case of color_based. If mix_and_match=1, then mismatch and color_based MUST be 0.

mismatch: ONLY valid for high_appeal treatment. Recommends avoiding matching nutrient colors to plant colors (blue, yellow, red), including both:
  (a) simple strategies (using non-corresponding colors), and
  (b) more complex strategies, such as the opposite of Mix & Match, that allocate remaining primary colors using color theory to contrast the plant’s existing color.
This category is a special case of color_based. If mismatch=1, then mix_and_match and color_based MUST be 0.

color_based: ONLY valid for high_appeal treatment. Recommends a strategy in which plant color is the primary cue for nutrient allocation (without explicit Mix & Match or Mismatch strategies). Includes simple color matching for primary colors only.


shape_based: Recommends a strategy in which plant shape is the primary cue for nutrient allocation, including feeding plants with the same shape in the same way.

spread: Recommends distributing nutrients broadly or evenly across plants or feeding each plant.

less_per_plant: Recommends allocating fewer nutrients to each plant without explicitly suggesting even distribution.

more_per_plant: Recommends allocating more nutrients to each plant.

mix_nutrients: Recommends combining multiple nutrient colors within a single plant. Don't classify if nutrient mixing is a part of another strategy (such as mix_and_match or mismatch) unless explicitly stated that mixing nutrients is beneficial by itself. (This category is about nutrient mixing in a plant, not about plant color.)

pure_nutrients: Recommends avoiding mixing nutrient colors within a plant. Don't classify if avoiding nutrient mixing is a part of another strategy unless explicitly stated that using nutrients in a single color only is beneficial by itself.

spatial: Recommends space-based strategies related to the field (e.g., feeding specific rows, columns, corners, or the center in specific ways).

useall: Recommends using all or nearly all 16 available nutrients per round.

time: Recommends strategies based on speed, timing, or temporal pacing.

blue_prioritize: Recommends explicitly prioritizing blue nutrients over other colors (or prioritizing blue equally with another nutrient color).

yellow_prioritize: Recommends explicitly prioritizing yellow nutrients over other colors (or prioritizing yellow equally with another nutrient color).

red_prioritize: Recommends explicitly prioritizing red nutrients over other colors (or prioritizing red equally with another nutrient color).

variation: Recommends diversifying allocations, randomizing, or changing strategies across trials.

other: Contains advice that does not fit into the predefined categories.

noinfo: Contains no actionable strategic information or refers only to the latest solution.

Rules:
- Output MUST be valid JSON matching the schema exactly.
- Use only integers 0 or 1.
- If text is noinfo, set noinfo=1 and all other categories 0.
- If advice is informative but fits none, set other=1.
- IMPORTANT validity rule:
  If treatment_appeal is NOT "high_appeal", then set mix_and_match=0 and mismatch=0, color_based=0 (even if the text mentions matching/mismatching).
"""


def build_text_format() -> Dict[str, Any]:
    # This matches the API's requirement: text.format.name must exist
    return {
        "type": "json_schema",
        "name": "advice_classification",
        "schema": {
            "type": "object",
            "additionalProperties": False,
            "properties": {k: {"type": "integer", "enum": [0, 1]} for k in CATEGORIES},
            "required": CATEGORIES,
        },
        "strict": True,
    }

def extract_advice_text(row: pd.Series) -> str:
    # Your template includes cleaned_feedback and feedback_message
    for col in ["cleaned_feedback", "feedback_message"]:
        if col in row and pd.notna(row[col]):
            txt = str(row[col]).strip()
            if txt and txt.lower() != "nan":
                return txt
    return ""

def make_all_zero(noinfo: bool = False) -> Dict[str, int]:
    out = {k: 0 for k in CATEGORIES}
    if noinfo:
        out["noinfo"] = 1
    return out

def enforce_non_high_appeal_rules(flags: Dict[str, int], treatment: str) -> Dict[str, int]:
    # As per your definition, mix_and_match & mismatch are only valid in high_appeal
    if (treatment or "").strip().lower() != "high_appeal":
        flags["mix_and_match"] = 0
        flags["mismatch"] = 0
    return flags

def call_model_row(
    client: OpenAI,
    model: str,
    treatment: str,
    advice_text: str,
    max_retries: int = 6,
    base_backoff: float = 1.0,
) -> Dict[str, int]:
    text_format = build_text_format()

    payload = {
        "treatment_appeal": treatment,
        "advice": advice_text,
    }

    for attempt in range(max_retries):
        try:
            resp = client.responses.create(
                model=model,
                temperature=TEMPERATURE,
                instructions=SYSTEM_INSTRUCTIONS,
                input=[
                    {"role": "user", "content": json.dumps(payload, ensure_ascii=False)}
                ],
                text={"format": text_format},
            )

            out_text = resp.output_text
            data = json.loads(out_text)

            # Validate keys & values
            for k in CATEGORIES:
                if k not in data:
                    raise ValueError(f"Missing key in model output: {k}")
                if data[k] not in (0, 1):
                    raise ValueError(f"Invalid value for {k}: {data[k]}")

            flags = {k: int(data[k]) for k in CATEGORIES}
            flags = enforce_non_high_appeal_rules(flags, treatment)

            # If noinfo=1, force all other 0 (hard rule)
            if flags.get("noinfo", 0) == 1:
                flags = make_all_zero(noinfo=True)

            # If nothing else is 1 and noinfo=0, set other=1
            if flags["noinfo"] == 0:
                informative_sum = sum(flags[k] for k in CATEGORIES if k not in ("other", "noinfo"))
                if informative_sum == 0:
                    flags["other"] = 1

            return flags

        except Exception as e:
            wait = base_backoff * (2 ** attempt)
            print(f"[warn] attempt {attempt+1}/{max_retries} failed: {e}. sleeping {wait:.1f}s")
            time.sleep(wait)

    raise RuntimeError("Max retries exceeded for a row.")

def get_row_key(row: pd.Series, fallback_index: int) -> str:
    # Your file has participant_code + chain_code + generation; use them if present
    parts = []
    for col in ["participant_code", "chain_code", "generation", "n"]:
        if col in row and pd.notna(row[col]):
            parts.append(f"{col}={row[col]}")
    if parts:
        return "|".join(parts)
    return f"index={fallback_index}"

def main() -> None:
    if not os.getenv("OPENAI_API_KEY"):
        raise SystemExit(
            "OPENAI_API_KEY is not set. Set it first: bash/zsh: export OPENAI_API_KEY='...'; "
            "PowerShell: $env:OPENAI_API_KEY='...'"
        )

    df = pd.read_csv(INPUT_PATH)

    # Ensure category columns exist and start at 0
    for c in CATEGORIES:
        if c not in df.columns:
            df[c] = 0
        df[c] = 0  # overwrite NaN/anything with 0 before we fill

    ckpt_path = os.path.splitext(OUTPUT_PATH)[0] + ".checkpoint.jsonl"

    done_keys = set()
    if os.path.exists(ckpt_path):
        with open(ckpt_path, "r", encoding="utf-8") as f:
            for line in f:
                try:
                    obj = json.loads(line)
                    done_keys.add(obj["row_key"])
                except Exception:
                    continue
        print(f"[info] loaded checkpoint: {len(done_keys)} rows already done")

    client = OpenAI()

    for i, row in df.iterrows():
        row_key = get_row_key(row, i)
        if row_key in done_keys:
            continue

        treatment = str(row.get("treatment_appeal", "")).strip()
        advice = extract_advice_text(row)

        if not advice:
            flags = make_all_zero(noinfo=True)
        else:
            flags = call_model_row(
                client=client,
                model=MODEL_NAME,
                treatment=treatment,
                advice_text=advice,
            )

        # Write to dataframe
        for k in CATEGORIES:
            df.at[i, k] = int(flags[k])

        # Append checkpoint
        with open(ckpt_path, "a", encoding="utf-8") as f:
            f.write(json.dumps({"row_key": row_key, "flags": flags}, ensure_ascii=False) + "\n")

        print(f"[ok] row {i} classified ({row_key})")
        if SLEEP_SECONDS > 0:
            time.sleep(SLEEP_SECONDS)

    df.to_csv(OUTPUT_PATH, index=False)
    print(f"[done] wrote: {OUTPUT_PATH}")
    print(f"[done] checkpoint: {ckpt_path}")

if __name__ == "__main__":
    main()
