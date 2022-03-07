Cats Effect
---

### The Effect Pattern
1. Тип програми повинен показати нами, 
які єфекти буде виконано додатково до типу, що буде повернуто. 
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

`IO.unit` те ж саме, що й `IO.pure(())`

### Error handling:

Якщо у `IO[A]` виникла помилка, а ви хочете:

- виконати ефект: використовуйте `onError(pf: PartialFunction[Throwable, IO[Unit]]): IO[A]`
- перетворити будь-яку помилку в іншу: `adaptError(pf: PartialFunction[Throwable, Throwable]): IO[A]`
- перетворити будь-яку помилку в успішне значення: `handleError(f: Throwable ⇒ A): IO[A]`
- перетворити деякі види помилок на успішне значення: `recover(pf: PartialFunction[Throwable, A]): IO[A]`
- трансформувати деякі види помилок в інший ефект: `recoverWith(pf: PartialFunction[Throwable, IO[A]]): IO[A]`
- зробити помилки видимими (присутніми у значенні `IO`), але відкласти обробку помилок: `IO[Either[Throwable, A]]`
- в іншому випадку використовуйте `handleErrorWith(f: Throwable ⇒ IO[A]): IO[A]`
