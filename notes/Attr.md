The `Attr` graph element must either use the regular `Observer` when used in-memory (as part of a `Control`, for
example); or it must use an `Event` if persisted as part of an ex-obj. Such an event must write and (de)serialise
a `Node`, which could be either the encompassing ex-obj, or the expanded attribute. The big disadvantage of the
former is that it could mean a large blob must be deserialised every time, and it will occupy space as multiple
attributes are used; also there must be a way to identify the `Attr`, i.e. one must map between event slots and
expanded attribute. In the latter case, the `Node` would probably just be expanded attribute. What remains unclear
is how the `ITargets` are stored or not stored and recreated. 

![illustration](attr-fire-2021-12-27-1134.png)

In this illustration, we assume an entry in the `AttrMap` is mapped; the regular `Event` chain is in black,
the red spot is the ex program's `Attr element; it would deserialise its dependents - red line; but it would also
need to ensure that its `ITargets` are deserialised and invoked, along the green line. Since
event filtering only happens in the pull phase, the push will automatically go through to the
ex obj's outlet, and then (black left pointing line) either to other expressions or to a live
observer in the UI.

---

Looking at this, I don't think it makes a lot of sense to introduce serialisation for `IExpr`; in any case we
would have to unwrap the whole thing. Let's restate the findings:

- it's possible to use an ad-hoc "headless" context to "run" the expression
- each `Event#react` should be captured for the ex obj to collect the events to listen to
- there is probably little benefit in making a complicated structure to propagate the event
- it is probably the best to simply evaluate the ex obj when an input `Event` fires
- in order to produce a `Change` update, we might need to cache the previous expression value
- but we might get around having to declare `Caching`, by just caching the last call to `value`?
  Because any sink connected would poll `value` initially before listening to events?
- as an optimisation, we could keep the new explicit `connect` methods on the `IExpr`s, and skip calling  
  them using a flag on the context. This would avoid a lot of setup when essentially the `IEvent` queue
  is never used

---

## Calls

- `Obj.Attr` -> `ExpandedObjAttr` -> `obj.changed.--->...`
- `Obs.Bridge.cellView` -> e.g. `ObjCellViewVarImpl`
- `Attr` if `selfOption.isDefined` -> `StmObjAttrMapCellView`
- `Attr` if `isNested` -> `StmObjCtxCellView` (`AbstractCtxCellView`)
- `Obj.Bridge.contextCellView` -> `AbstractCtxCellView`

## Dynamic

The problematic bit is updating the objects that are observed. Say `"in".attr(0)`, and say that entry already
exists. Now we would gather in the test expansion the event listeners for the attribute map as well as for
the particular `IntObj` found in the attribute map's entry. Now the object is replaced, eventually resulting
in a `pullUpdate` of the wrapping ex obj. This would cause in some form of caching `IExpr` a kind of
`setObj` method to be called, disposing the old listener and installing a new listener. Basically the
ex obj has a list of events it listens to, and the call to `value` or `pullUpdate` produces a new such list;
this list may be different from the old list; in that case, we would have to unregister `oldList diff newList`,
and newly register `newList diff oldList`.
