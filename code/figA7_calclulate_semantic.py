import re
from pathlib import Path

import numpy as np
import pandas as pd
from sentence_transformers import SentenceTransformer


INPUT_PATH = Path("data/df_long.csv")
OUT_DIR = Path("data/processed")
MODEL_NAME = "all-MiniLM-L6-v2"


def clean_text(text: str) -> str:
    if not isinstance(text, str):
        return ""
    text = re.sub(
        r"^\s*the field yields a higher output when\s*",
        "",
        text,
        flags=re.IGNORECASE,
    )
    return text.strip()


def similarity_stats(embeddings: np.ndarray) -> dict:
    n = embeddings.shape[0]
    if n < 2:
        return {
            "n": n,
            "n_pairs": 0,
            "mean_similarity": np.nan,
            "median_similarity": np.nan,
            "sd_similarity": np.nan,
            "min_similarity": np.nan,
            "max_similarity": np.nan,
        }

    sim = embeddings @ embeddings.T
    upper = sim[np.triu_indices(n, k=1)]

    return {
        "n": n,
        "n_pairs": len(upper),
        "mean_similarity": float(np.mean(upper)),
        "median_similarity": float(np.median(upper)),
        "sd_similarity": float(np.std(upper, ddof=1)),
        "min_similarity": float(np.min(upper)),
        "max_similarity": float(np.max(upper)),
    }


def main() -> None:
    OUT_DIR.mkdir(parents=True, exist_ok=True)

    df = pd.read_csv(INPUT_PATH)

    required_cols = {
        "period",
        "generation",
        "treatment_appeal",
        "chain_code",
        "feedback_message",
    }
    missing_cols = sorted(required_cols - set(df.columns))
    if missing_cols:
        raise ValueError(f"Missing required columns in df_long: {', '.join(missing_cols)}")

    df = df[df["period"] == 6].copy()
    df["generation"] = df["generation"].astype(int)
    df["feedback_message_clean"] = df["feedback_message"].apply(clean_text)
    df["row_id"] = np.arange(len(df))

    model = SentenceTransformer(MODEL_NAME)
    embeddings = model.encode(
        df["feedback_message_clean"].tolist(),
        batch_size=32,
        normalize_embeddings=True,
        convert_to_numpy=True,
        show_progress_bar=True,
    )

    def group_stats(group: pd.DataFrame) -> pd.Series:
        emb = embeddings[group["row_id"].values]
        return pd.Series(similarity_stats(emb))

    by_treatment_generation = (
        df.groupby(["treatment_appeal", "generation"])
        .apply(group_stats, include_groups=False)
        .reset_index()
    )

    by_treatment = (
        df.groupby(["treatment_appeal"])
        .apply(group_stats, include_groups=False)
        .reset_index()
    )

    by_chain_generation = (
        df.groupby(["chain_code", "generation"])
        .apply(group_stats, include_groups=False)
        .reset_index()
    )

    by_chain = (
        df.groupby(["chain_code"])
        .apply(group_stats, include_groups=False)
        .reset_index()
    )

    by_treatment_generation.to_csv(
        OUT_DIR / "semantic_similarity_by_treatment_generation.csv", index=False
    )
    by_treatment.to_csv(OUT_DIR / "semantic_similarity_by_treatment.csv", index=False)
    by_chain_generation.to_csv(
        OUT_DIR / "semantic_similarity_by_chain_generation.csv", index=False
    )
    by_chain.to_csv(OUT_DIR / "semantic_similarity_by_chain.csv", index=False)

    print(f"Saved semantic summary inputs to: {OUT_DIR}")


if __name__ == "__main__":
    main()
