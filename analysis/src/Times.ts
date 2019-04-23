import { fmap_null } from "fmap-null-undefined";
import * as fs from "fs";

function isSomething<T>(x: T | null): x is T {
  return x !== null;
}

const r = String.raw;

type Time = number

type UnitString = "ns" | "μs" | "ms" | "s" // not a regular µ, it seems

const TIME = r`(\d+(?:\.\d+)?) ([nμm]?s)`;

const REGEX_WHITESPACE_PLUS_TIME = new RegExp(r`^time\s+` + TIME, "mg");

const REGEX_ONLY_TIME = new RegExp(TIME);

function getTimes(raw: string): ReadonlyArray<Time> {
  const matchResult = raw.match(REGEX_WHITESPACE_PLUS_TIME);
  const result = fmap_null((res: RegExpMatchArray) => {
    const times = res.map(m => m.match(REGEX_ONLY_TIME));
    return times.filter(isSomething).map(t => normalize(parseFloat(t[1]), t[2] as UnitString));
  })(matchResult);
  return result === null ? [] : result;
}

main();

function main() {
  const all: Array<ReadonlyArray<Time>> = [];
  for (let i = 1; i <= 20; i++) {
    const filename = `./scaling/benchmark-kmeans-(2019-04-22_19-56)-scaling-${i.toString().padStart(2, "0")}.log`;
    const content = fs.readFileSync(filename);
    all.push(getTimes(content.toString()));
  }
  all.forEach(x => {
    console.log(x.join("\t"))
  });
}

function normalize(n: number, unit: UnitString): Time {
  switch (unit) {
    case "ns": return n * 1e-6;
    case "μs": return n * 1e-3; // not a regular µ, it seems
    case "ms": return n;
    case "s":  return n * 1e3;
  }
}

