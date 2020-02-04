module VDom = {
  type vnode;
  type attr;

  [@bs.module "../../../src/util"]
  external combine: (Js.Dict.t('a), Js.Dict.t(Dom.event => unit)) => attr =
    "combine";

  type attribute('a) =
    | Attr(string, 'a)
    | Event(string, Dom.event => unit);

  let createAttributeParam = (attrs: list(attribute('a))): attr => {
    let attributes = Js.Dict.empty();
    let events = Js.Dict.empty();

    attrs
    |> List.iter(x =>
         switch (x) {
         | Attr(name, value) => Js.Dict.set(attributes, name, value)
         | Event(name, handler) => Js.Dict.set(events, "on" ++ name, handler)
         }
       );

    combine(attributes, events);
  };

  [@bs.module "superfine"]
  external bindedh: (string, attr, array(vnode)) => vnode = "h";

  let h =
      (
        name: string,
        ~attrs: list(attribute('a))=[],
        ~children: list(vnode)=[],
        (),
      )
      : vnode => {
    let attrs = createAttributeParam(attrs);

    bindedh(name, attrs, Belt.List.toArray(children));
  };

  [@bs.module "../../../src/util"] external text: string => vnode = "through";

  [@bs.module "superfine"]
  external patch: (Dom.element, vnode) => Dom.element = "patch";
};

let h = VDom.h;
let text = VDom.text;

let component =
    (
      factory: ('state, 'state => unit) => VDom.vnode,
      defaultState: 'state,
      node: Dom.element,
    ) => {
  let rec dispatch = state => {
    let vdom =
      factory(
        state,
        state => {
          dispatch(state);
          ();
        },
      );

    VDom.patch(node, vdom);
  };

  dispatch(defaultState);
};

let render = (component, node: Dom.element) => component(node);

let attr = (name, value) => VDom.Attr(name, value);
let event = (name, handler) => VDom.Event(name, handler);
