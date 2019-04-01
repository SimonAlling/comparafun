import { fmap_null } from "fmap-null-undefined";
import * as fs from "fs";

const r = String.raw;

type Maybe<T> = T | null

interface BenchmarkResult {
  n : number
  k : number
  seed : number
  mode : Mode
  partitions : number
  time : number // ms
  stddev : number
}

interface SuiteResult {
  threads : number
  hecs : number
  chunks : ReadonlyArray<Chunk>
}

type Chunk = ReadonlyArray<BenchmarkResult>

type Time = number

type Mode = "seq" | "par"

type UnitString = "ns" | "μs" | "ms" | "s" // not a regular µ, it seems

const SUITE_DIVIDER = "-------- SUITE";
const BENCHMARK_DIVIDER = "-------- BENCHMARK";

const REGEX_ARCH = new RegExp([
  r`Detected (\d+) hardware threads`,
  r`Using (\d+) HECs`,
].join(r`\n`));

const REGEX_CONFIG = new RegExp([
  r`n = (\d+)`,
  r`k = (\d+)`,
  r`seed = (\d+)`,
  r`partitions = (\d+)`,
].join(r`\n`), "m");

const WHITESPACE_PLUS_TIME = r`\s+(\d+(?:\.\d+)?) ([nμm]?s)`;

const REGEX_TIME = new RegExp(r`^time` + WHITESPACE_PLUS_TIME, "m");

const REGEX_STDDEV = new RegExp(r`^std dev` + WHITESPACE_PLUS_TIME, "m");

const REGEX_MODE = /benchmarking kmeans_(seq|par)/;

main();

function main() {
  if (process.argv.length > 2) { // 0 = interpreter; 1 = script
    // Arguments exist.
    fs.readFile(process.argv[2], null, (err, data) => {
      if (err === null) {
        console.warn(showOutput(parseOutput(data.toString())));
      } else {
        console.error(err.message);
      }
    });
  } else {
    console.warn("No filename given.");
  }
}

function showOutput(results: ReadonlyArray<SuiteResult>): string {
  return results.map(showSuiteResult).join("\n\n");
}

function showSuiteResult(result: SuiteResult): string {
  const firstChunk = result.chunks[0];
  const bestChunk = result.chunks.reduce((acc, chunk) => bestSpeedup(acc) > bestSpeedup(chunk) ? acc : chunk);
  const worstChunk = result.chunks.reduce((acc, chunk) => worstSpeedup(acc) < worstSpeedup(chunk) ? acc : chunk);
  return [
    ``,
    `Hardware threads: ` + result.threads,
    `Haskell Execution Contexts (HECs): ` + result.hecs,
    ``,
    [`n`, `k`, `seed`].join("\t") + `\t\tSeq.\t` + firstChunk.slice(1).map(x => x.partitions).join("\t") + "\t\tSpeedup",
    ``,
    result.chunks.map(showChunk).join("\n\n"),
    ``,
    `Best:`,
    showChunk(bestChunk),
    `Worst:`,
    showChunk(worstChunk),
  ].join("\n");
}

function showChunk(chunk: Chunk): string {
  return [
    showConfig(chunk[0]),
    times(chunk).join("\t"),
    worstSpeedup(chunk).toFixed(1) + "  –\t" + bestSpeedup(chunk).toFixed(1),
  ].join("\t\t");
}

function showConfig(c: { k: number, n: number, seed: number }): string {
  return [c.n, c.k, c.seed].join("\t");
}

function times(chunk: Chunk): ReadonlyArray<Time> {
  return chunk.map(c => c.time);
}

// Assumes that the first time is the sequential one.
function sequentialOver(reduce: (xs: ReadonlyArray<number>) => number): (chunk: Chunk) => number {
  return chunk => times(chunk)[0] / reduce(times(chunk).slice(1));
}

const bestSpeedup = sequentialOver(minimum);
const worstSpeedup = sequentialOver(maximum);

function parseOutput(raw: string): ReadonlyArray<SuiteResult> {
  const suites = raw.split(SUITE_DIVIDER).map(s => s.trim()).filter(notEmpty).map(parseSuite).filter(isDefined);
  if (suites.length === 0) throw new Error("No suites found.");
  return suites;
}

function parseSuite(raw: string): Maybe<SuiteResult> {
  const match_arch = raw.match(REGEX_ARCH);
  if (match_arch === null) throw new Error("Architecture parse failed.");
  return {
    threads: parseInt(match_arch[1]),
    hecs: parseInt(match_arch[2]),
    chunks: splitOnSeq(parseBenchmarks(raw)),
  };
}

function parseBenchmarks(raw: string): ReadonlyArray<BenchmarkResult> {
  const results = raw.split(BENCHMARK_DIVIDER).map(parseIndividualBenchmark).filter(isDefined);
  const dividers = (raw.match(new RegExp(BENCHMARK_DIVIDER, "g")) || []).length;
  if (results.length !== dividers) {
    throw new Error(`Found ${dividers} divider(s), but ${results.length} result(s).`);
  }
  return results;
}

function parseIndividualBenchmark(raw: string): Maybe<BenchmarkResult> {
  return (
    fmap_null((m_config: RegExpMatchArray) =>
    fmap_null((m_time: RegExpMatchArray) =>
    fmap_null((m_stddev: RegExpMatchArray) =>
    fmap_null((m_mode: RegExpMatchArray) =>
      ({
          n: parseInt(m_config[1]),
          k: parseInt(m_config[2]),
          seed: parseInt(m_config[3]),
          mode: m_mode[1] as Mode,
          partitions: parseInt(m_config[4]),
          time: Math.round(normalize(parseFloat(m_time[1]), m_time[2] as UnitString)),
          stddev: Math.round(normalize(parseFloat(m_stddev[1]), m_stddev[2] as UnitString)),
      })
    )(raw.match(REGEX_MODE))
    )(raw.match(REGEX_STDDEV))
    )(raw.match(REGEX_TIME))
    )(raw.match(REGEX_CONFIG))
  );
}

function splitOnSeq(results: ReadonlyArray<BenchmarkResult>): ReadonlyArray<Chunk> {
  const lists = [];
  let current = [] as Array<BenchmarkResult>;
  results.forEach(result => {
    if (result.mode === "seq" && current.length > 0) {
      lists.push(current);
      current = [];
    }
    current.push(result);
  });
  lists.push(current);
  return lists;
}

function normalize(n: number, unit: UnitString): number {
  switch (unit) {
    case "ns": return n * 1e-6;
    case "μs": return n * 1e-3; // not a regular µ, it seems
    case "ms": return n;
    case "s":  return n * 1e3;
  }
}

function minimum(xs: ReadonlyArray<number>): number {
  return xs.reduce((acc, x) => Math.min(acc, x), Infinity);
}

function maximum(xs: ReadonlyArray<number>): number {
  return xs.reduce((acc, x) => Math.max(acc, x), -Infinity);
}

function isDefined<T>(x: T | null | undefined): x is T {
  return x !== null && x !== undefined;
}

function notEmpty(s: string): boolean {
  return s !== "";
}
