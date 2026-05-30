interface Foo {
  bar: Bar;
  baz: string;
  greet(name: string): string;
  link(other: Bar): Foo;
}

interface Bar {
  qux: Foo;
  count: number;
}
