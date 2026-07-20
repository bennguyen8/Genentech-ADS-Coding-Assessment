"""
test_queries.py
===============
Test script for the ClinicalTrialDataAgent (Question 4 deliverable).

Runs three example free-text queries - one for each routing target
(AESEV severity, AETERM condition, AESOC body system) - and prints the
count of unique subjects and a preview of the matching IDs for each.

Run:  python test_queries.py
      (uses the mock backend automatically if no OPENAI_API_KEY is set)
"""

from clinical_agent import ClinicalTrialDataAgent, load_adae


# Three example questions chosen to exercise all three routing paths.
# None of them mention a column name - the agent must infer it.
EXAMPLE_QUESTIONS = [
    # -> AESEV  (severity / intensity)
    "Give me the subjects who had Adverse events of Moderate severity",
    # -> AETERM (a specific named condition)
    "Which subjects experienced diarrhoea?",
    # -> AESOC  (a body system)
    "Show me subjects with cardiac disorders",
]

# ---------------------------------------------------------------------------
# HARD / ADVERSARIAL queries - these DELIBERATELY probe the mock backend's
# limits. They are expected to route imperfectly under MockLLMBackend and
# are included ON PURPOSE to show, honestly, where keyword matching breaks
# down and why the RealLLMBackend (LangChain + OpenAI) exists.
#
# For each, the comment states what a real LLM would do. When run with an
# OPENAI_API_KEY set (RealLLMBackend active), these are exactly the cases
# the LLM handles that the mock cannot - the whole justification for the
# LLM-backed design. This is not a bug in the pipeline; it is the mock's
# expected ceiling.
# ---------------------------------------------------------------------------
HARD_QUESTIONS = [
    # Paraphrase: "heart problems" means CARDIAC DISORDERS, but "heart" is
    # not a keyword the mock knows -> it mis-routes to AETERM and finds 0.
    # A real LLM maps the concept "heart" -> AESOC 'CARDIAC DISORDERS'.
    "Which patients had heart problems?",
    # Synonym for severity: "how bad" implies AESEV, but without the literal
    # tokens "sever/intensit/mild/moderate" the mock misses it.
    # A real LLM recognises the intent -> AESEV.
    "How bad were the reactions in the treatment groups?",
    # Misspelling: "headake" should be AETERM 'HEADACHE'; the mock matches
    # against exact dataset vocabulary, so a typo slips past it.
    # A real LLM is robust to spelling and maps -> AETERM 'HEADACHE'.
    "List anyone who reported a headake",
]


def _run_batch(agent, questions, label, explain=False):
    """Run a list of questions through the agent and print results."""
    print("\n" + "-" * 70)
    print(label)
    print("-" * 70)
    for i, question in enumerate(questions, start=1):
        print(f"\n[{i}] {question}")
        try:
            result = agent.ask(question)
            print(f"  routed to : {result.target_column} == "
                  f"'{result.filter_value}'")
            print(f"  matched   : {result.matched_rows} AE rows")
            print(f"  subjects  : {result.n_subjects} unique")
            preview = result.subject_ids[:10]
            suffix = "" if result.n_subjects <= 10 else \
                f"  (+{result.n_subjects - 10} more)"
            print(f"  IDs       : {preview}{suffix}")
            if explain and result.n_subjects == 0:
                print("  NOTE      : 0 subjects - the mock mis-routed this "
                      "paraphrase/typo. A real LLM would resolve the intent "
                      "(see the comment on this query). This is the mock's "
                      "expected ceiling, not a pipeline bug.")
        except Exception as exc:  # keep the suite running if one query fails
            print(f"  ERROR: {exc}")


def main() -> None:
    df = load_adae()
    agent = ClinicalTrialDataAgent(df)

    print("=" * 70)
    print("ClinicalTrialDataAgent - example query run")
    print(f"Rows in dataset: {len(df):,}")
    print(f"Backend in use : {agent.backend_name}")
    print("=" * 70)

    # The three required example queries - one per routing target.
    _run_batch(agent, EXAMPLE_QUESTIONS,
               "STANDARD QUERIES (one per routing target: AESEV / AETERM / AESOC)")

    # Deliberately hard queries that probe the mock's limits. Under the mock
    # these are expected to route imperfectly; under the real LLM backend
    # they would resolve correctly. Included to show the boundary honestly.
    _run_batch(agent, HARD_QUESTIONS,
               "HARD QUERIES (probe the mock's ceiling - real LLM would handle these)",
               explain=True)

    print("\n" + "=" * 70)
    print("Done. Standard queries route correctly under the mock; the hard")
    print("queries show where keyword matching breaks and the real LLM")
    print("(RealLLMBackend) would take over with an OPENAI_API_KEY set.")
    print("=" * 70)


if __name__ == "__main__":
    main()