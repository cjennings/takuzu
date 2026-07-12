#!/usr/bin/env node
// Replay tests/fixtures/parity-cases.json against the JS engine embedded in
// docs/prototypes/takuzu-hifi.html (the #takuzu-engine script block), so the
// Elisp engine and its HTML port can't silently drift.  No dependencies.
"use strict";

import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { dirname, join } from "node:path";
import vm from "node:vm";

const root = join(dirname(fileURLToPath(import.meta.url)), "..");
const htmlPath = join(root, "docs", "prototypes", "takuzu-hifi.html");
const casesPath = join(root, "tests", "fixtures", "parity-cases.json");

const html = readFileSync(htmlPath, "utf8");
const m = html.match(/<script id="takuzu-engine">([\s\S]*?)<\/script>/);
if (!m) {
  console.error(`no <script id="takuzu-engine"> block in ${htmlPath}`);
  process.exit(1);
}

const ctx = vm.createContext({});
vm.runInContext(m[1], ctx, { filename: "takuzu-engine.js" });
const E = vm.runInContext("TakuzuEngine", ctx);

const cases = JSON.parse(readFileSync(casesPath, "utf8")).cases;

// Mirror the fixture generator: grade and solution are only computed for
// uniquely-solvable boards; forced is the first row-major single-value cell.
function evaluate(c) {
  const n = c.size;
  const cells = E.parseCells(c.cells);
  const unique = E.isUnique(cells, n);
  const solution = unique ? E.solve(cells, n) : null;
  return {
    legal: E.boardLegal(cells, n),
    full: E.boardFull(cells),
    solved: E.boardSolved(cells, n),
    unique,
    grade: unique ? E.grade(cells, n) : null,
    forced: E.forcedCell(cells, n),
    solution: solution ? E.cellsString(solution) : null,
  };
}

const sameForced = (a, b) =>
  (a == null && b == null)
  || (a != null && b != null && a.length === 3 && b.length === 3
      && a.every((v, i) => v === b[i]));

let failures = 0;
for (const c of cases) {
  const got = evaluate(c);
  const diffs = [];
  for (const key of ["legal", "full", "solved", "unique", "grade", "solution"])
    if (got[key] !== c[key])
      diffs.push(`${key}: expected ${JSON.stringify(c[key])}, got ${JSON.stringify(got[key])}`);
  if (!sameForced(c.forced, got.forced))
    diffs.push(`forced: expected ${JSON.stringify(c.forced)}, got ${JSON.stringify(got.forced)}`);
  if (diffs.length) {
    failures++;
    console.log(`FAIL ${c.name}: ${diffs.join("; ")}`);
  }
}

if (failures) {
  console.log(`parity: ${failures}/${cases.length} cases failed`);
  process.exit(1);
}
console.log(`parity: all ${cases.length} cases pass`);
