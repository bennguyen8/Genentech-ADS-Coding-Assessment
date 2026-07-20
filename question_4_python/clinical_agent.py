"""
clinical_agent.py
=================
ADS Programmer Coding Assessment - Question 4 (bonus): GenAI Clinical
Data Assistant.

A `ClinicalTrialDataAgent` that turns a clinical reviewer's free-text
question about the adverse-event dataset into a structured query and runs
it against the data with pandas.

Flow (Prompt -> Parse -> Execute), exactly as the assessment requires:
    1. PROMPT : the user's natural-language question + a schema description
                of the relevant AE columns is sent to an LLM.
    2. PARSE  : the LLM returns a *structured* object
                {target_column, filter_value} (validated by Pydantic).
    3. EXECUTE: that structured query is applied as a real pandas filter,
                returning the count of unique subjects and their IDs.

LLM backend (two interchangeable implementations behind one interface):
    - RealLLMBackend : LangChain + OpenAI, using
      ChatOpenAI(...).with_structured_output(Query, method="json_schema").
      Used automatically when an OPENAI_API_KEY is available and langchain
      is installed.
    - MockLLMBackend : a deterministic, dependency-free stand-in that
      returns the SAME {target_column, filter_value} shape via keyword
      routing. Used automatically when no API key is present, so the whole
      pipeline runs end-to-end for a grader without a key.

Because both backends implement the same `.route(question) -> Query`
method, the agent's Prompt->Parse->Execute logic is identical regardless
of which one is active - only the "understanding" step swaps out.
"""

from __future__ import annotations

import os
from dataclasses import dataclass, field
from typing import Optional

import pandas as pd

try:
    # Pydantic is a light dependency and is used for the structured schema
    # in BOTH backends (the mock returns a Query too), so import at top.
    from pydantic import BaseModel, Field
except ImportError as exc:  # pragma: no cover
    raise ImportError(
        "pydantic is required: pip install pydantic"
    ) from exc


# ---------------------------------------------------------------------------
# 1. SCHEMA DEFINITION
# ---------------------------------------------------------------------------
# Description of the AE columns the agent is allowed to route to. This dict
# is injected into the LLM prompt so the model can map user intent (which
# may not mention column names at all) to the correct variable. Keeping it
# as data (not hard-coded rules) is the whole point of the exercise: to add
# a new routable column, you edit this dict, not the routing logic.
AE_SCHEMA: dict[str, str] = {
    "AESEV": (
        "Severity or intensity of the adverse event. "
        "Values: 'MILD', 'MODERATE', 'SEVERE'. "
        "Use for questions about how bad / how severe / intensity of AEs."
    ),
    "AETERM": (
        "The verbatim reported name of the specific adverse event / "
        "medical condition (e.g. 'HEADACHE', 'NAUSEA', 'DIARRHOEA'). "
        "Use for questions naming a particular symptom or condition."
    ),
    "AESOC": (
        "System Organ Class - the body system grouping of the adverse "
        "event (e.g. 'CARDIAC DISORDERS', 'SKIN AND SUBCUTANEOUS TISSUE "
        "DISORDERS'). Use for questions about a body system or organ."
    ),
    "AEBODSYS": (
        "Body system or organ class (often same grouping as AESOC). "
        "Secondary option for body-system questions if AESOC is absent."
    ),
}


# The structured output contract. The LLM (or mock) must return exactly
# these two fields; Pydantic validates them client-side so a malformed
# response fails fast and clearly.
class Query(BaseModel):
    """Structured representation of a user's AE question."""

    target_column: str = Field(
        description="The dataset column to filter on (e.g. AESEV, AETERM, AESOC)."
    )
    filter_value: str = Field(
        description="The value to search for within target_column "
        "(e.g. 'MODERATE', 'HEADACHE')."
    )


# ---------------------------------------------------------------------------
# 2. LLM BACKENDS (one interface, two implementations)
# ---------------------------------------------------------------------------
class LLMBackend:
    """Interface: turn a natural-language question into a validated Query."""

    def route(self, question: str) -> Query:  # pragma: no cover - interface
        raise NotImplementedError


class RealLLMBackend(LLMBackend):
    """LangChain + OpenAI structured-output implementation.

    Uses ChatOpenAI(...).with_structured_output(Query, method="json_schema"),
    the current LangChain pattern for guaranteed schema-valid output. The
    schema description is passed in the system prompt so the model can map
    intent -> column without any hard-coded rules.
    """

    def __init__(self, schema: dict[str, str], model: str = "gpt-4o-mini"):
        # Imports are local so that importing this module never *requires*
        # langchain unless the real backend is actually used.
        from langchain_openai import ChatOpenAI
        from langchain_core.prompts import ChatPromptTemplate

        self._schema = schema
        schema_text = "\n".join(f"- {col}: {desc}" for col, desc in schema.items())

        self._prompt = ChatPromptTemplate.from_messages(
            [
                (
                    "system",
                    "You are a clinical data assistant. Map the user's "
                    "question to exactly one dataset column and the value "
                    "to filter for. Return only the structured fields.\n\n"
                    "Available columns:\n" + schema_text + "\n\n"
                    "Rules: choose the single most appropriate target_column. "
                    "Normalise filter_value to match the data's conventions "
                    "(AESEV values are upper-case MILD/MODERATE/SEVERE; "
                    "AETERM/AESOC values are upper-case).",
                ),
                ("human", "{question}"),
            ]
        )
        # temperature=0 for deterministic, reproducible routing.
        llm = ChatOpenAI(model=model, temperature=0)
        self._chain = self._prompt | llm.with_structured_output(
            Query, method="json_schema"
        )

    def route(self, question: str) -> Query:
        return self._chain.invoke({"question": question})


class MockLLMBackend(LLMBackend):
    """Deterministic stand-in used when no API key is available.

    Mirrors what the LLM would decide, using simple keyword routing, and
    returns the SAME Query shape. This is transparently a stand-in for the
    LLM's "understanding" step - in production RealLLMBackend replaces it
    with zero changes to the agent's Prompt->Parse->Execute flow. It is NOT
    pretending to be an LLM; it exists so the pipeline runs without a key.
    """

    # Known vocabularies let the mock normalise values the way the LLM
    # would (e.g. "moderate" -> "MODERATE", "cardiac" -> the full SOC).
    _SEVERITIES = {"MILD", "MODERATE", "SEVERE"}

    def __init__(self, schema: dict[str, str], data: Optional[pd.DataFrame] = None):
        self._schema = schema
        # If given the data, learn the real AETERM/AESOC vocabularies so the
        # mock can match user words to actual values present in the dataset.
        self._terms = (
            set(data["AETERM"].dropna().str.upper()) if data is not None
            and "AETERM" in data else set()
        )
        self._socs = (
            set(data["AESOC"].dropna().str.upper()) if data is not None
            and "AESOC" in data else set()
        )

    def route(self, question: str) -> Query:
        q = question.upper()

        # (a) severity / intensity -> AESEV
        if any(w in q for w in ("SEVER", "INTENSIT", "MILD", "MODERATE")):
            value = next((s for s in self._SEVERITIES if s in q), "MODERATE")
            return Query(target_column="AESEV", filter_value=value)

        # (b) explicit body-system words, or a known SOC string -> AESOC
        soc_hit = next((s for s in self._socs if s in q), None)
        if soc_hit or any(w in q for w in ("BODY SYSTEM", "ORGAN", "CARDIAC",
                                           "SKIN", "GASTRO", "NERVOUS")):
            value = soc_hit or _first_soc_for_keyword(q, self._socs)
            if value:
                return Query(target_column="AESOC", filter_value=value)

        # (c) a named condition matching a known AETERM -> AETERM
        term_hit = next((t for t in self._terms if t in q), None)
        if term_hit:
            return Query(target_column="AETERM", filter_value=term_hit)

        # (d) fallback: assume it's a condition name; take the last capitalised
        # token(s) from the original question as the search value.
        return Query(target_column="AETERM", filter_value=question.strip().upper())


def _first_soc_for_keyword(q: str, socs: set[str]) -> Optional[str]:
    """Map a loose body-system keyword to a full SOC value if possible."""
    keyword_map = {
        "CARDIAC": "CARDIAC DISORDERS",
        "SKIN": "SKIN AND SUBCUTANEOUS TISSUE DISORDERS",
        "GASTRO": "GASTROINTESTINAL DISORDERS",
        "NERVOUS": "NERVOUS SYSTEM DISORDERS",
    }
    for kw, soc in keyword_map.items():
        if kw in q and soc.upper() in socs:
            return soc
    return None


# ---------------------------------------------------------------------------
# 3. THE AGENT (Prompt -> Parse -> Execute)
# ---------------------------------------------------------------------------
@dataclass
class QueryResult:
    """What a query returns to the caller."""

    question: str
    target_column: str
    filter_value: str
    n_subjects: int
    subject_ids: list[str] = field(default_factory=list)
    matched_rows: int = 0

    def __repr__(self) -> str:
        preview = ", ".join(self.subject_ids[:5])
        more = "" if len(self.subject_ids) <= 5 else f", ... (+{len(self.subject_ids) - 5})"
        return (
            f"QueryResult(q={self.question!r} -> {self.target_column}="
            f"{self.filter_value!r}: {self.n_subjects} subjects, "
            f"{self.matched_rows} rows; ids=[{preview}{more}])"
        )


class ClinicalTrialDataAgent:
    """Routes free-text AE questions to a pandas filter via an LLM (or mock)."""

    def __init__(
        self,
        data: pd.DataFrame,
        schema: dict[str, str] = AE_SCHEMA,
        backend: Optional[LLMBackend] = None,
        subject_col: str = "USUBJID",
    ):
        self.data = data
        self.schema = schema
        self.subject_col = subject_col

        if subject_col not in data.columns:
            raise ValueError(
                f"Subject column {subject_col!r} not found in data. "
                f"Available: {list(data.columns)[:10]}..."
            )

        # Backend selection: explicit override wins; otherwise use the real
        # LLM when a key is present, else the deterministic mock.
        if backend is not None:
            self.backend = backend
            self.backend_name = type(backend).__name__
        elif os.environ.get("OPENAI_API_KEY"):
            try:
                self.backend = RealLLMBackend(schema)
                self.backend_name = "RealLLMBackend"
            except ImportError:
                # Key present but langchain not installed -> fall back.
                self.backend = MockLLMBackend(schema, data)
                self.backend_name = "MockLLMBackend (langchain not installed)"
        else:
            self.backend = MockLLMBackend(schema, data)
            self.backend_name = "MockLLMBackend (no OPENAI_API_KEY)"

    # --- PROMPT + PARSE ---------------------------------------------------
    def interpret(self, question: str) -> Query:
        """Send the question to the backend and get a validated Query."""
        query = self.backend.route(question)
        # Guard: the routed column must exist in the data.
        if query.target_column not in self.data.columns:
            raise ValueError(
                f"LLM routed to column {query.target_column!r}, which is not "
                f"in the dataset. Available AE columns: "
                f"{[c for c in self.schema if c in self.data.columns]}"
            )
        return query

    # --- EXECUTE ----------------------------------------------------------
    def execute(self, query: Query) -> QueryResult:
        """Apply the structured query as a pandas filter."""
        col = self.data[query.target_column].astype("string").str.upper()
        mask = col == query.filter_value.upper()
        subset = self.data.loc[mask]

        ids = sorted(subset[self.subject_col].dropna().unique().tolist())
        return QueryResult(
            question="",  # filled in by ask()
            target_column=query.target_column,
            filter_value=query.filter_value,
            n_subjects=len(ids),
            subject_ids=ids,
            matched_rows=len(subset),
        )

    # --- PUBLIC API -------------------------------------------------------
    def ask(self, question: str) -> QueryResult:
        """Full pipeline: interpret (Prompt->Parse) then execute."""
        query = self.interpret(question)
        result = self.execute(query)
        result.question = question
        return result


# ---------------------------------------------------------------------------
# Convenience loader
# ---------------------------------------------------------------------------
def load_adae(path: str = "adae.csv") -> pd.DataFrame:
    """Load the AE dataset exported from pharmaversesdtm::ae."""
    return pd.read_csv(path)


if __name__ == "__main__":
    # Minimal smoke test when run directly (the full 3-query test suite is
    # in test_queries.py).
    df = load_adae()
    agent = ClinicalTrialDataAgent(df)
    print(f"Backend in use: {agent.backend_name}\n")
    print(agent.ask("Give me the subjects who had Adverse events of Moderate severity"))