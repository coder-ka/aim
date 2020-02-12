type componentType =
  (unit, Dom.element, bool, ~marker: option(Dom.comment), unit) => unit;

type eventOption = {
  capture: bool,
  once: bool,
  passive: bool,
};

module ElementOperation = {
  open Webapi.Dom;

  let svgNamespace = "http://www.w3.org/2000/svg";

  let createElement = (name: string, isSvg: bool): Dom.element => {
    isSvg
      ? Document.createElementNS(svgNamespace, name, document)
      : Document.createElement(name, document);
  };
  let setAttribute =
      (el: Dom.element, name: string, value: string, isSvg: bool) => {
    isSvg
      ? Element.setAttributeNS(svgNamespace, name, value, el)
      : Element.setAttribute(name, value, el);
  };
  let removeAttribute = (el: Dom.element, name: string, isSvg: bool) => {
    isSvg
      ? Element.removeAttributeNS(svgNamespace, name, el)
      : Element.removeAttribute(name, el);
  };
};

module DynamicPart = {
  type keyedDynamicPart('state, 'item) = {
    key: string,
    parts: dynamicParts('state, 'item),
  }
  and dynamicParts('state, 'item) = {
    node: option(Dom.node),
    dynamics: list(dynamicPart('state, 'item)),
  }
  and dynamicPart('state, 'item) =
    | DynamicTextNodePart({
        el: Dom.node,
        func: 'state => string,
        current: string,
      })
    | DynamicNodesPart({
        el: Dom.element,
        iterator: 'state => array('item),
        keyFn: ('item, int) => string,
        dynamicsFn: ('item, int) => dynamicParts('state, 'item),
        start: Dom.comment,
        ending: Dom.comment,
        current: option(array(keyedDynamicPart('state, 'item))),
      })
    | DynamicStringAttributePart({
        el: Dom.element,
        name: string,
        func: 'state => string,
        current: option(string),
      })
    | DynamicBooleanAttributePart({
        el: Dom.element,
        name: string,
        func: 'state => bool,
        current: option(bool),
      });

  let rec updateDynamic =
          (
            root: Dom.element,
            state: 'state,
            isSvg: bool,
            dynamic: dynamicPart('state, 'item),
          ) => {
    Webapi.Dom.(
      switch (dynamic) {
      | DynamicTextNodePart({el, func, current}) =>
        let content = func(state);

        if (content !== current) {
          el->Node.setTextContent(content);
        };

        DynamicTextNodePart({el, func, current: content});
      | DynamicNodesPart({
          el,
          iterator,
          keyFn,
          dynamicsFn,
          start,
          ending,
          current,
        }) =>
        let parent = el;

        let iterable = iterator(state);

        let current =
          switch (current) {
          | Some(current) =>
            let newList =
              iterable
              |> Array.mapi((i, x) => {
                   let key = keyFn(x, i);
                   let item = current |> Js.Array.find(x => x.key === key);

                   switch (item) {
                   | Some(item) =>
                     let {parts} = item;

                     switch (parts.node) {
                     | Some(node) =>
                       parent |> Element.insertBefore(node, ending);

                       List.map(
                         updateDynamic(root, state, isSvg),
                         parts.dynamics,
                       );

                       ();
                     | None => ()
                     };

                     item;
                   | None =>
                     let {node, dynamics} = dynamicsFn(x, i);

                     List.map(updateDynamic(root, state, isSvg), dynamics);

                     {
                       key,
                       parts: {
                         node,
                         dynamics,
                       },
                     };
                   };
                 });

            // remove unneccesary nodes
            let firstItem =
              Array.length(newList) > 0 ? Some(newList[0]) : None;

            switch (firstItem) {
            | Some({parts}) =>
              switch (parts.node) {
              | Some(node) =>
                let rec remove = pre => {
                  switch (pre) {
                  | Some(pre) =>
                    if (pre !== (start |> Comment.asNode)) {
                      parent |> Element.removeChild(pre);
                      remove(node |> Webapi.Dom.Node.previousSibling);
                    }

                  | None => ()
                  };
                };

                remove(node |> Webapi.Dom.Node.previousSibling);
              | None => ()
              }
            | None => ()
            };

            newList;
          | None =>
            iterable
            |> Array.mapi((i, x) => {
                 let {node, dynamics} = dynamicsFn(x, i);

                 List.map(updateDynamic(el, state, isSvg), dynamics);

                 {
                   key: keyFn(x, i),
                   parts: {
                     node,
                     dynamics,
                   },
                 };
               })
          };

        DynamicNodesPart({
          el,
          iterator,
          keyFn,
          dynamicsFn,
          start,
          ending,
          current: Some(current),
        });
      | DynamicStringAttributePart({el, name, func, current}) =>
        let value = func(state);
        switch (current) {
        | Some(current) =>
          if (value !== current) {
            ElementOperation.setAttribute(el, name, value, isSvg);
          }
        | None => ElementOperation.setAttribute(el, name, value, isSvg)
        };

        DynamicStringAttributePart({el, name, func, current: Some(value)});
      | DynamicBooleanAttributePart({el, name, func, current}) =>
        let value = func(state);

        let set = (el, name, value) =>
          if (value) {
            ElementOperation.setAttribute(el, name, name, isSvg);
          } else {
            ElementOperation.removeAttribute(el, name, isSvg);
          };

        switch (current) {
        | Some(current) =>
          if (value !== current) {
            set(el, name, value);
          }
        | None => set(el, name, value)
        };

        DynamicBooleanAttributePart({el, name, func, current: Some(value)});
      }
    );
  };
};

module Attribute = {
  type attribute('state, 'mutation) =
    | DynamicAttribute(dynamicAttribute('state))
    | StaticAttribute(staticAttribute('mutation))
  and dynamicAttribute('state) =
    | DynamicStringAttribute(string, 'state => string)
    | DynamicBooleanAttribute(string, 'state => bool)
  and staticAttribute('mutation) =
    | StaticStringAttribute(string, string)
    | StaticBooleanAttribute(string, bool)
    | StaticEvent(string, (Dom.event, 'mutation => unit) => unit)
    | StaticEventWithOptions(
        string,
        (Dom.event, 'mutation => unit) => unit,
        eventOption,
      );

  open DynamicPart;

  let setAttribute =
      (
        el: Dom.element,
        isSvg: bool,
        attributes: list(attribute('state, 'mutation)),
        dispatch: 'mutation => unit,
      )
      : dynamicParts('state, 'item) => {
    open Webapi.Dom;
    let dynamics =
      attributes
      |> List.map(attribute =>
           switch (attribute) {
           | DynamicAttribute(attribute) =>
             switch (attribute) {
             | DynamicStringAttribute(name, func) => [
                 DynamicStringAttributePart({el, name, func, current: None}),
               ]
             | DynamicBooleanAttribute(name, func) => [
                 DynamicBooleanAttributePart({el, name, func, current: None}),
               ]
             }
           | StaticAttribute(attribute) =>
             switch (attribute) {
             | StaticStringAttribute(name, value) =>
               ElementOperation.setAttribute(el, name, value, isSvg);
               [];
             | StaticBooleanAttribute(name, value) =>
               if (value) {
                 ElementOperation.setAttribute(el, name, name, isSvg);
               } else {
                 ElementOperation.removeAttribute(el, name, isSvg);
               };
               [];
             | StaticEvent(name, handler) =>
               el |> Element.addEventListener(name, e => handler(e, dispatch));

               [];
             | StaticEventWithOptions(name, handler, eventOption) =>
               el
               |> Element.addEventListenerWithOptions(
                    name,
                    e => handler(e, dispatch),
                    {
                      "capture": eventOption.capture,
                      "once": eventOption.once,
                      "passive": eventOption.passive,
                    },
                  );
               [];
             }
           }
         )
      |> List.flatten;

    {node: Some(el |> Element.asNode), dynamics};
  };
};

module Node = {
  open Attribute;

  type node('state, 'item, 'mutation) =
    | DynamicNode(dynamicNode('state, 'item, 'mutation))
    | StaticNode(staticNode('state, 'item, 'mutation))
  and dynamicNode('state, 'item, 'mutation) =
    | DynamicTextNode('state => string)
    | DynamicNodes(
        'state => array('item),
        ('item, int) => string,
        ('item, int) => node('state, 'item, 'mutation),
      )
  and staticNode('state, 'item, 'mutation) =
    | StaticTextNode(string)
    | StaticElement(
        string,
        list(attribute('state, 'mutation)),
        list(node('state, 'item, 'mutation)),
        bool,
      )
    | StaticSlot(componentType);

  open Webapi.Dom;
  open DynamicPart;

  let createMarker = () => Document.createComment("", document);

  let rec append =
          (
            root: Dom.element,
            node: node('state, 'item, 'mutation),
            container: Dom.element,
            isSvg: bool,
            dispatch: 'mutation => unit,
            ~marker: option(Dom.comment),
            (),
          )
          : dynamicParts('state, 'item) => {
    let start = createMarker();
    container |> Element.appendChild(start);

    let ending =
      switch (marker) {
      | Some(marker) => marker
      | None =>
        let marker = createMarker();
        container |> Element.appendChild(marker);
        marker;
      };

    switch (node) {
    | DynamicNode(node) =>
      switch (node) {
      | DynamicTextNode(func) =>
        let text =
          container
          |> Element.insertBefore(
               Document.createTextNode("", document),
               ending,
             );

        {
          node: Some(text |> Text.asNode),
          dynamics: [
            DynamicTextNodePart({el: text |> Text.asNode, func, current: ""}),
          ],
        };
      | DynamicNodes(iterator, keyFn, childFn) => {
          node: None,
          dynamics: [
            DynamicNodesPart({
              el: container,
              iterator,
              keyFn,
              dynamicsFn: (item, i) =>
                append(
                  root,
                  childFn(item, i),
                  container,
                  isSvg,
                  dispatch,
                  ~marker=Some(ending),
                  (),
                ),
              start,
              ending,
              current: None,
            }),
          ],
        }
      }
    | StaticNode(node) =>
      switch (node) {
      | StaticTextNode(content) =>
        let text =
          container
          |> Element.insertBefore(
               document |> Document.createTextNode(content),
               ending,
             );

        {node: Some(text |> Text.asNode), dynamics: []};
      | StaticElement(name, attrs, nodes, isSvg) =>
        let el =
          container
          |> Element.insertBefore(
               ElementOperation.createElement(name, isSvg),
               ending,
             );

        let marker = createMarker();
        el |> Element.appendChild(marker);

        let attrs = setAttribute(el, isSvg, attrs, dispatch).dynamics;
        let children =
          List.flatten(
            nodes
            |> List.map(node =>
                 append(root, node, el, isSvg, dispatch, ~marker=None, ()).
                   dynamics
               ),
          );

        {
          node: Some(el |> Element.asNode),
          dynamics: List.flatten([attrs, children]),
        };
      | StaticSlot(component) =>
        component((), container, isSvg, ~marker=Some(ending), ());

        {node: None, dynamics: []};
      }
    };
  };
};

module Template = {
  open DynamicPart;

  let create =
      (
        nodes: list(Node.node('state, 'item, 'mutation)),
        root: Dom.element,
        isSvg: bool,
        defaultState: 'state,
        action: ('state => unit, 'mutation, 'state) => unit,
        ~marker: option(Dom.comment),
        (),
      ) => {
    let dynamics = ref(None);
    let currentState = ref(defaultState);

    let update = state => {
      currentState := state;
      switch (dynamics^) {
      | Some(x) =>
        dynamics :=
          Some(
            List.map(
              dynamic =>
                DynamicPart.updateDynamic(root, state, isSvg, dynamic),
              x,
            ),
          )
      | None => ()
      };
    };

    let dispatch = mutation => {
      action(update, mutation, currentState^);
    };

    let parts =
      nodes
      |> List.map(node => {
           let parts =
             Node.append(root, node, root, isSvg, dispatch, ~marker, ());

           parts.dynamics;
         })
      |> List.flatten;

    dynamics := Some(parts);

    update(defaultState);

    update;
  };
};

let component =
    (
      nodes: list(Node.node('state, 'item, 'mutation)),
      defaultState: 'state,
      action: ('state => unit, 'mutation, 'state) => unit,
      created: ('state => unit) => unit,
      (),
      container: Dom.element,
      isSvg: bool,
      ~marker: option(Dom.comment),
      (),
    ) => {
  let update =
    Template.create(
      nodes,
      container,
      isSvg,
      defaultState,
      action,
      ~marker,
      (),
    );

  created(update);
};

let statelessComponent =
    (
      nodes: list(Node.node('state, 'item, 'mutation)),
      created: unit => unit,
      (),
      container: Dom.element,
      isSvg: bool,
      ~marker: option(Dom.comment),
      (),
    ) => {
  let update =
    Template.create(
      nodes,
      container,
      isSvg,
      (),
      (_, _, _) => (),
      ~marker,
      (),
    );

  created();
};

let render = (component: componentType, el: Dom.element) =>
  component((), el, false, ~marker=None, ());

open Node;

let html = (name, attrs, children) =>
  StaticNode(StaticElement(name, attrs, children, false));
let svg = (name, attrs, children) =>
  StaticNode(StaticElement(name, attrs, children, true));

let text = content => StaticNode(StaticTextNode(content));
let text_ = func => DynamicNode(DynamicTextNode(func));
let nodes_ = (iterator, keyFn, childFn) =>
  DynamicNode(DynamicNodes(iterator, keyFn, childFn));
let slot = component => StaticNode(StaticSlot(component));

open Attribute;

let attr = (name, value) =>
  StaticAttribute(StaticStringAttribute(name, value));
let attr_ = (name, func) =>
  DynamicAttribute(DynamicStringAttribute(name, func));

let boolAttr = (name, value) =>
  StaticAttribute(StaticBooleanAttribute(name, value));
let boolAttr_ = (name, func) =>
  DynamicAttribute(DynamicBooleanAttribute(name, func));

let event = (name, handler) => StaticAttribute(StaticEvent(name, handler));
let eventWithOptions = (name, handler, eventOption) =>
  StaticAttribute(StaticEventWithOptions(name, handler, eventOption));

// store
module type State = {
  type state;
  let initialState: state;
};

module Store = (State: State) => {
  let current = ref(State.initialState);
  let hooks = ref([]);
  let subscribe = hook => {
    hooks := [hook, ...hooks^];
  };

  let commit = state => {
    current := state;
    hooks^ |> List.iter(hook => hook(state));
  };
};
