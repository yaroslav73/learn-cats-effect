Cats Effect
---
---

### IO

`IO.pure` - обчислюється відразу, parameter is passed by value (eagerly evaluated).

`IO.apply` - обчислюється ліниво, parameter is passed by name (lazily evaluated).

`IO.unit` те ж саме, що й `IO.pure(())`
