interface Play {
  scene: Scene;
  script: string;
  greet(name: string): string;
  ping(): string;
  link(other: Scene): Play;
  tags: string[];
  counts: number[];
  nickname?: string;
  id: string | number;
  lookup: Map<number, string>;
}

interface Scene {
  play: Play;
  count: number;
}
