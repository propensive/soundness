interface Foo {
  bar: Bar;
  baz: string;
  greet(name: string): string;
  link(other: Bar): Foo;
  tags: string[];
  nickname?: string;
  id: string | number;
  lookup: Map<number, string>;
}

interface Bar {
  qux: Foo;
  count: number;
}
