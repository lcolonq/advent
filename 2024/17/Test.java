record Prod(int i, int j) {
}

sealed interface Either<A, B> {};
record Left<A, B>(A val) implements Either<A, B> {}
record Right<A, B>(B val) implements Either<A, B> {}

void main(String[] args) {
    Either<String, Integer> e = new Left<>("yo");
    e = new Right<>(10);
    switch (e) {
    case Left(var val) -> System.out.println(val);
    case Right(var val) -> System.out.println(String.format("%d", val + 1));
    }
}
