// A big Board, as a busy day leaves it: one lead with 60 需要 Jing lines
// open over 20 work items, from 23 reports (a report's lines share its
// time); another with 7 over 3 items, from 2; an agent at a question --
// what stops it.  63 items in all: 31 with something going on, 32 at
// rest; PRs and tickets on many.  As Emacs sends it (boardPatch) and the
// Inbox's push (renderInbox).  Made-up work, made-up names.
const now = Date.now();
const min = 60e3;
const P = (leaf) => "~/src/" + leaf + "/";
const hex = (n, w) => n.toString(16).padStart(w || 6, "0");

const LEAD = "lead-alpha", LEAD2 = "lead-beta", AGY = "agy-gamma";
const AGENTS = [
  { name: LEAD, project: P("alpha"), role: "lead" }, { name: "rd-alpha", project: P("alpha"), role: "" },
  { name: LEAD2, project: P("beta"), role: "lead" }, { name: "rd-beta", project: P("beta"), role: "" },
  { name: AGY, project: P("gamma"), role: "" }, { name: "lead-gamma", project: P("gamma"), role: "lead" },
];

const pr = (repo, id, state, i) => ({ host: "bitbucket", owner: "acme", repo, id, state, title: repo + " change " + id, dest: "master",
  url: "https://bitbucket.org/acme/" + repo + "/pull-requests/" + id, draft: false, unsure: false, own: true, of: "",
  first: now - (300 + i) * min, last: now - (30 + i) * min });

// The lines: the newest report first.  Lead-alpha's 23 reports, its 60
// lines spread over them (3, 3, 3, 2, 3, 2 ...), each report's lines on
// one of 20 items; lead-beta's 2 reports, 7 lines on 3 items.
const NEEDS = [];
let ev = 100;
function lines(agent, reports, total, items, itemBase, startAgo, every, words) {
  let left = total;
  for (let r = 0; r < reports; r++) {
    const n = r === reports - 1 ? left : Math.max(1, Math.min(left - (reports - r - 1), r % 3 === 0 ? 3 : 2 + (r % 2)));
    left -= n;
    const at = now - (startAgo + r * every) * min;
    for (let k = 0; k < n; k++) {
      const item = itemBase + hex((r * 7 + k) % items, 4);
      const id = ++ev;
      NEEDS.push({ id, kind: "need", agent, at, ask: r % 4 === 1 ? "a" + hex(id, 7) : null, item,
                   text: words[(r + k) % words.length] + "（第 " + (r + 1) + " 則報告之 " + (k + 1) + "）" });
    }
  }
}
const W1 = ["決定 sandbox 要不要關掉訪客登入", "確認 builder3 可不可以用來跑夜間建置", "核准 #812 依序 merge 並上線",
  "換掉兩個環境的 dashboard 管理密碼", "跟 Mia 確認 cache-proxy 是誰開的", "決定 #809 的 a1b2c3d 要不要留",
  "用自己的帳號從公司的機器掃一次 port", "決定 plugin 目錄要不要拿掉"];
const W2 = ["決定 chart 要不要升版", "核准 replica 改成 3", "看一下 canary 的錯誤率再決定"];
lines(LEAD, 23, 60, 20, "a1", 19, 52, W1);
lines(LEAD2, 2, 7, 3, "b2", 95, 300, W2);

const itemTitle = (id) => (id.startsWith("b2") ? "beta 的工作 " : "alpha 的工作 ") + id;
const card = (id, status, extra) => Object.assign({
  id, status, title: itemTitle(id), named: false, labels: [], asks: 3, open: 0, fyi: 0, acks: 0, relays: 0, failed: 0,
  first: now - 900 * min, last: now - 60 * min, agents: [], projects: [], cross: false, prs: [], links: [], jingOf: [],
  needs: [], deploys: [], unanswered: null, tickets: [],
  block: { kind: "answered", ask: "aa" + id, from: "rd-alpha", to: LEAD, since: now - 60 * min, head: "做好了，等 lead 看" }, closed: null }, extra || {});

const CARDS = [];
// The 23 items with lines open: waiting on Jing -- quiet four hours and
// more, long (`late', 等你 N 小時); one's last ask went nowhere besides,
// still its lead's wait, not nobody's.
const byItem = {};
NEEDS.forEach((n) => (byItem[n.item] = byItem[n.item] || []).push(n));
Object.keys(byItem).forEach((id, i) => {
  const ns = byItem[id], alpha = id.startsWith("a1");
  const last = Math.max(...ns.map((n) => n.at));
  CARDS.push(card(id, "jing", {
    asks: 6 + (i % 9), last, late: last < now - 240 * min, agents: alpha ? [LEAD, "rd-alpha"] : [LEAD2, "rd-beta"], projects: alpha ? ["alpha", "infra"] : ["beta"],
    cross: alpha, labels: i % 3 ? ["CU-86" + hex(i, 4)] : [],
    needs: ns.map((n) => ({ id: n.id, kind: "need", text: n.text, agent: n.agent, ask: n.ask, at: n.at })),
    prs: [pr(alpha ? "alpha" : "beta", 400 + i, i % 3 ? "OPEN" : "MERGED", i), pr("infra", 600 + i, "MERGED", i)],
    deploys: i % 4 ? [] : [{ id: 900 + i, kind: "deploy", text: "alpha #" + (400 + i) + " → staging", agent: LEAD, ask: null, at: last }],
    unanswered: i === 5 ? { ask: "dead" + hex(i, 4), from: LEAD, to: "rd-alpha", status: "stopped", at: last, head: "把 proxy 的設定整理成表" } : null,
  }));
});
// 8 more going on, 32 at rest.
for (let i = 0; i < 8; i++) CARDS.push(card("c3" + hex(i, 4), i % 2 ? "live" : "tail", { asks: 4, open: i % 2, last: now - (10 + i * 20) * min,
  agents: [LEAD, "rd-alpha"], projects: ["alpha"], prs: [pr("alpha", 500 + i, "OPEN", i)], labels: ["CU-86" + hex(40 + i, 4)],
  block: i % 2 ? { kind: "open", ask: "op" + hex(i, 6), from: LEAD, to: "rd-alpha", since: now - (10 + i) * min, status: "delivered", working: true,
    head: "把 retry 的上限改成 5 次" } : undefined }));
for (let i = 0; i < 32; i++) CARDS.push(card("d4" + hex(i, 4), "quiet", { asks: i % 5 ? 1 : 4, last: now - (100 + i * 30) * min,
  agents: [LEAD2], projects: [i % 2 ? "beta" : "gamma"], prs: i % 4 ? [] : [pr("beta", 700 + i, "MERGED", i)] }));
CARDS.forEach((c) => (c.tickets = (c.labels || []).map((l) => ({ id: l.slice(3), last: c.last }))));

const JING = NEEDS.slice().sort((a, b) => b.at - a.at).map((n) => ({ id: "need:" + n.id, event: n.id, kind: "need", tag: "需要 Jing", line: true,
  item: n.item, itemTitle: itemTitle(n.item), title: n.text, agent: n.agent, at: n.at, ask: n.ask,
  replyVia: "lead", projectDir: n.agent === LEAD ? P("alpha") : P("beta") }));
const WAITS = [0, 1, 2].map((i) => ({ ask: "op" + hex(i * 2 + 1, 6), item: "c3" + hex(i * 2 + 1, 4), from: LEAD, to: "rd-alpha",
  fromProj: "alpha", toProj: "alpha", kind: "ask", status: "delivered", since: now - (11 + i) * min, working: true, under: null,
  head: "把 retry 的上限改成 5 次" }));
const META = { counts: { asks: 240, xproj: 30, fyi: 12, relays: 3, failed: 2 }, edges: [{ from: "alpha", to: "infra", relay: false, n: 9 }],
  days: 1, more: true, agents: AGENTS };
const TICKETS = CARDS.flatMap((c) => c.tickets).filter((t, i, a) => a.findIndex((x) => x.id === t.id) === i)
  .map((t) => ({ id: t.id, name: "單號 " + t.id, status: "in progress", type: "custom", color: "#4194f6", url: "https://app.clickup.com/t/" + t.id,
    error: null, done: false, loading: false, unsure: false }));
const BIG = { v: 1, base: 0, full: true, cards: CARDS, drop: [], waits: WAITS, jing: JING, meta: META, tickets: TICKETS };

// The Inbox's push: each line as its item (ghostherd-inbox--need-items),
// and the question that stops agy-gamma.
const INBOX = NEEDS.map((n) => ({ id: "need:" + n.id, kind: "need", group: "need", tag: "需要 Jing", tone: "red", agent: n.agent, cli: "",
  project: n.agent === LEAD ? P("alpha") : P("beta"), title: n.text.length > 79 ? n.text.slice(0, 79) + "…" : n.text, preview: n.text,
  at: n.at, read: false, override: null, whyLabel: "A lead's 需要 Jing line",
  body: n.text + "\n\nWork item " + n.item + ": " + itemTitle(n.item), event: n.id, item: n.item, itemTitle: itemTitle(n.item), ask: n.ask || "" }))
  .concat([{ id: "question:" + AGY + ":1", kind: "question", group: "need", tag: "Question", tone: "accent", agent: AGY, cli: "agy",
    project: P("gamma"), title: "這件事可以分三層來看：要不要加上回放？", preview: "這件事可以分三層來看：要不要加上回放？",
    at: now - 8 * 1440 * min, read: false, override: null, whyLabel: "", body: "這件事可以分三層來看：要不要加上回放？", replyVia: "agent" }]);

module.exports = { BIG, CARDS, NEEDS, INBOX, AGENTS, LEAD, LEAD2, AGY, P, now };
