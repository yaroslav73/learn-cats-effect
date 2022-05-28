Cats Effect
---

### Що таке effect?

### The Effect Pattern
1. Тип програми повинен показати нами,
   які ефекти буде виконано додатково до типу, що буде повернуто.
2. Якщо бажана поведінка покладається на якийсь видимий ззовні `side-effect` (побічні ефекти),
   ми відокремлюємо опис ефектів, які ми хочемо виконати, від їх фактичного виконання.
   Ми можемо вільно замінювати опис ефектів до моменту, коли ми їх запускаємо.

#### Effect Pattern Checklist
1. Чи говорить тип програми:
   1. які ефекти буде виконувати програма;
   2. який тип значення буде повернуто?
2. Коли потрібні зовнішні видимі `side-effect`, чи є опис ефекту окремо від виконання?

### IO

`IO.pure` - обчислюється відразу, parameter is passed by value (eagerly evaluated).

`IO.apply` - обчислюється ліниво, parameter is passed by name (lazily evaluated).

`IO.unit` - те ж саме, що й `IO.pure(())`

### Parallel IO
1. IO не має паралельності - чому?
2. Як зробити IO паралельним?
3. Parallel тайп-клас
4. parMapN, parTraverse, parSequence в чому різниця? Типи цих методів та лінки на приклади.
5. У чому різниця запуску паралельного IO.


Тези та висновки:
1. IO does not support parallel operations itself, because it is a Monad.
2. The Parallel typeclass specifies the translation between a pair of effect types:
   one that is a Monad and the other that is “only” an Applicative.
3. Parallel[IO] connects the IO effect to its parallel counterpart, IO.Par.
4. Parallel IO composition requires the ability to shift computations to other threads within the current ExecutionContext. This is how parallelism is “implemented”.
5. parMapN, parTraverse, parSequence are the parallel versions of (the sequential) mapN, traverse, and sequence. Errors are managed in a fail-fast manner.
6. Note that sequence and traverse are mutually definable: x.sequence is x.traverse(identity), and x.traverse(f) is x.map(f).sequence

### Concurrent Control


### Shifting Context

Паралелізм (`parallelism`) використовує набір ресурсів для виконання ефектів.
Для JVM це пул потоків (`thread pool`): ефекти виконуються на доступних потоках одночасно.
Основною абстракцією Scala для використання пулів потоків є `scala.concurrent.ExecutionContext`,
і `Cats Effect` використовує його для реалізації паралельності та конкурентності (`concurrency`).

### Integrating Asynchrony


### Managing resources

Тип даних `Resource` представляє шаблон `acquire-use-release` для того щоб управляти станом (`state`).  
Спочатку ми здобуваємо (`acquire`) ресурс, потім використовуємо (`use`) його, і після цього "відпускаємо" (`release`).

Створити ресурс можна за допомогою `Resource.make`, але давайте поглянемо на сигнатуру `make`:
```scala
def make[A](acquire: IO[A])(release: A => IO[Unit]): Resource[IO, A]
```
Отже в нас є `acquire` та `release`, два з трьох компонентів нашого шаблону `acquire-use-release`.
Ми описали як отримаємо ресурс, що зробимо під час його "звільнення" - залишилось лише використати його.


### Concurrent coordination

`Fiber` представляє ефект, що вже виконується. З `Fiber` можна почати контролювати конкурентні
ефекти приєднуючись (`join`) або скасовуючи (`cancel`) їх.

Але є ще така штука як координація (`coordination`) між конкурентними ефектами. Під координацією
розуміють, що поведінка одного ефекту повинна залежати від іншого.

`Ref` - тип даних для спільного використання змінюваного стану.

Створення ефекту за допомогою фабричного методу `Ref[IO].of(...)` повертає `Ref` як ефект - `IO[Ref[IO, Long]]`

`Deferred` - тип даних, що може забезпечити конкурентну серіалізацію ефекту, не блокуючи
жодних фактичних потоків.


### Error handling:

Якщо у `IO[A]` виникла помилка, а ви хочете:

- виконати ефект: використовуйте `onError(pf: PartialFunction[Throwable, IO[Unit]]): IO[A]`
- перетворити будь-яку помилку в іншу: `adaptError(pf: PartialFunction[Throwable, Throwable]): IO[A]`
- перетворити будь-яку помилку в успішне значення: `handleError(f: Throwable ⇒ A): IO[A]`
- перетворити деякі види помилок на успішне значення: `recover(pf: PartialFunction[Throwable, A]): IO[A]`
- трансформувати деякі види помилок в інший ефект: `recoverWith(pf: PartialFunction[Throwable, IO[A]]): IO[A]`
- зробити помилки видимими (присутніми у значенні `IO`), але відкласти обробку помилок: `IO[Either[Throwable, A]]`
- в іншому випадку використовуйте `handleErrorWith(f: Throwable ⇒ IO[A]): IO[A]`
