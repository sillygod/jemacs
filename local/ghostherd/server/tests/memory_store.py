"""A shared-memory store shaped like deploy-platform's, for the tests of
the opening, the facts' index and what asks carry: 33 facts -- four
feedback rules, a shared user fact, project and reference facts, English
and Chinese -- their words written for the test.  Embeddings are the fake
ones (hashed bags of tokens; conftest), so `meaning' here is overlap; the
real model's tuning is in memory_rank's docstring."""

import pytest

import shared_memory as sm
from engine import reset_engine

PROJECT = "/work/deploy-platform"
KEY = sm.slug_for_path(PROJECT)

RULES = {
    "branch-discipline": (
        "Always work on a feature branch in deploy-platform; never commit or push to main.\n\n"
        "**Why:** a background git sync moves HEAD back to main between turns, and twice a "
        "commit landed on main because of it.\n\n"
        "**How to apply:** run `git branch --show-current` right before every commit and push; "
        "on main, `git checkout -b <feature-branch>` first.  PRs merge by rebase, never a merge commit."),
    "pod-env-grep-leaks-secrets": (
        "Never `env | grep` a pattern inside a deploy-platform pod: the environment is full of "
        "tokens.\n\n**How to apply:** ask for named variables, or read the code and config files."),
    "clickup-ticket-upkeep": (
        "Keep ClickUp subtasks and their parent in sync at every PR milestone -- opened, review "
        "fixed, merged -- statuses and comments both, without being asked."),
    "prefer-resume-over-drain": (
        "For background work a deploy would kill, persist it and resume it on a live pod; do not "
        "hold terminating backend pods with a long grace period."),
}
USER_RULE = ("prefers-reasoned-design",
             "Jing wants a researched design proposal with its reasoning, not a menu of options, "
             "when asking how something should be designed.")

PROJECT_FACTS = [
    ("uv-lock-must-follow-pyproject", "deploy-platform backend 動 pyproject.toml dependencies 就必須跑 uv lock，否則只有 image build 會炸。"),
    ("vm-playbook-nginx-certs", "deploy-platform 執行 unnoiac deploy_nginx.yml（ZeroSSL 憑證續簽）via dagger；憑證回寫是正確性的必要條件。"),
    ("alert-diagnosis-model", "告警排查（alert_diagnostic）的預設 model 換成 gemini-3.8-flash，和 sentry_diagnostic 共用 run_model 設定鏈。"),
    ("dagger-deploy-gcp-credentials", "How dagger deploys authenticate to GKE, why an unbaked token silently becomes dagger/default, and the staging service account that reaches prod."),
    ("deploy-trigger-dispatch", "The three deploy-trigger entry points and the shared executor-aware dispatcher that picks dagger or drone for each deploy."),
    ("deploy-status-helm-data-path", "How the /deployments status page gets live helm data; the WG cluster versus the deploy-platform cluster."),
    ("gemini-token-spend", "Where Gemini 3.8-flash tokens go: ai-runner about 44 percent, pr_review implicit-cache misses, untracked callers."),
    ("sentry-autofix-direction", "Sentry to AI diagnosis to a PR: sentry.io SaaS is the target, not the self-hosted one; Seer only covers the GitHub half."),
    ("code-chat-write-mode", "Code-intel chat's write mode -- edit and open a PR, run_check -- is live; ai-runner is on Autopilot with a storage cap per pod."),
    ("cluster-probe-tools", "code_chat's kubectl cluster-probe tools: read-only first, approved exec second, and why the security posture is deliberate."),
    ("cn-prod-alert-paths", "Why a cn-prod problem may never reach the Alerts page: Alertmanager is a null receiver and only labelled Grafana rules route."),
    ("cn-stg-alert-threshold-tuning", "cn-stg alert noise was a `for:` duration problem, not a threshold problem; what was measured and rejected."),
    ("code-intel-chat-direction", "Direction for the code-intel UI: native chat over the code graph, Gemini, cross-repo groups."),
    ("gitnexus-ai-review-phase0", "State and how to test the GitNexus code-graph AI review pilot, phase 0."),
    ("ai-agent-framework", "The AI agent framework's phases 1 and 2 shipped; the roadmap for phases 3 and 4 and the design decisions behind it."),
    ("rbac-phase3-reassignment", "RBAC phase 3: reassigning the non-owner admins waits for two weeks of usage data; where the evidence lives."),
    ("gcm-cloudsql-tools", "code_chat's Cloud SQL monitoring tools through Google Cloud Monitoring: gating, prod credentials, Query Insights."),
    ("pr-review-cross-repo-search", "The pr_review agent's cross-repo search tools, for finding a service's consumers in other repos."),
]
REFERENCE_FACTS = [
    ("agent-trace-analysis", "How to measure code_chat and pr_review agent runs from ai_agent_runs and ai_agent_steps without fooling yourself."),
    ("bitbucket-bbql-state-filter", "The Bitbucket pullrequests API ignores state= whenever q= is present: put state=\"OPEN\" inside the BBQL."),
    ("bitbucket-pr-auth", "Authenticate the bitbucket-pr-manager skill with an API token and an email, not a username."),
    ("clickup-list-id-vs-team-id", "ClickUp task URLs start with the team id, not a list id; a subtask needs the parent's list.id from the API."),
    ("cn-stg-metrics-querying", "How to query cn-stg Prometheus and Cloud SQL metrics ad hoc, and the two facts that make naive queries return nothing."),
    ("deployed-api-testing", "How to call authenticated deploy-platform APIs on the deployed staging cluster without handling any secret."),
    ("diagnostic-prompt-eval", "How to A/B a sentry_diagnostic prompt or model read-only in a pod, with a time cut-off so the agent cannot read the merged fix."),
    ("gemini-implicit-cache-threshold", "Gemini's implicit cache only reuses a growing conversation once the previous request is past about 16k tokens."),
    ("kctx-multi-cluster", "Use `kctx <ctx-substring> <kubectl args>` for any GKE cluster: it picks the right gcloud service account per call."),
    ("platform-log-forensics", "How to prove what happened to a deploy-platform HTTP request hours later: Loki and Prometheus in the staging cluster."),
]

DEPLOY_ASK = """Ship the dagger deploy fix.

Commit it on a feature branch, push the branch and open the PR for review; once it is merged, trigger the deploy to staging and confirm the pods picked it up."""
UNRELATED_ASKS = ["Write a short poem about autumn leaves falling in the park.",
                  "幫我寫一首關於秋天的詩"]


def _day(i: int) -> str:
    return f"2026-10-{1 + i % 28:02d}T09:00:00Z"


@pytest.fixture
def store(tmp_path, monkeypatch):
    "The store, written; the sidecar's data (Qdrant) in a temp dir too."
    monkeypatch.setenv("GHOSTHERD_SHARED_MEMORY_DIR", str(tmp_path / "memory"))
    monkeypatch.setenv("GHOSTHERD_MEMORY_DIR", str(tmp_path / "sidecar"))
    reset_engine()
    path = sm.project_store(KEY)
    for i, (name, text) in enumerate(RULES.items()):
        sm.write_fact(path, KEY, text, type="feedback", name=name, now=_day(i))
    sm.write_fact(sm.user_store(), sm.USER_KEY, USER_RULE[1], type="user",
                  name=USER_RULE[0], now=_day(3))
    for i, (name, text) in enumerate(PROJECT_FACTS):
        sm.write_fact(path, KEY, text, type="project", name=name, now=_day(i))
    for i, (name, text) in enumerate(REFERENCE_FACTS):
        sm.write_fact(path, KEY, text, type="reference", name=name, now=_day(i))
    yield path
    reset_engine()


# What the tests expect, stated here rather than asked of the code under test.
RULE_NAMES = set(RULES) | {USER_RULE[0]}
RULE_BODIES = list(RULES.values()) + [USER_RULE[1]]
OTHER_NAMES = {n for n, _ in PROJECT_FACTS + REFERENCE_FACTS}


def rules():
    return [f for f in sm.pool(KEY) if f.name in RULE_NAMES]


def others():
    return [f for f in sm.pool(KEY) if f.name in OTHER_NAMES]


def whole_line(f):
    "A fact's index line, as MEMORY.md writes it."
    title = f.name.replace("-", " ")
    title = title[:1].upper() + title[1:]
    return f"- [{title}]({f.name}.md) — {f.description}"


def note_is_whole(note, more):
    """NOTE names facts of MORE, whole, in order, then perhaps `… and K more'
    for the rest -- never a name cut short."""
    head = f"{len(more)} more, not listed — "
    assert note.startswith(head)
    if note.endswith("."):
        return []                            # not one name fitted
    names = note.split("for any: ", 1)[1]
    if " … and " in names:
        names, tail = names.split(" … and ", 1)
        named = names.split(", ")
        assert tail == f"{len(more) - len(named)} more"
    else:
        named = names.split(", ")
        assert len(named) == len(more)
    assert named == more[: len(named)]
    return named


# The herd an ask goes through: the lead, a claude and a grok of the project.
SNAP = [
    {"name": "lead", "kind": "claude", "state": "idle", "project": PROJECT},
    {"name": "claude-dp", "kind": "claude", "state": "idle", "project": PROJECT},
    {"name": "grok-dp", "kind": "grok", "state": "idle", "project": PROJECT},
]
