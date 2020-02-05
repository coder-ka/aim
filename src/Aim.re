type component =
  (unit, Dom.element, bool, ~marker: option(Dom.comment), unit) => unit;

type eventOption = {
  capture: bool,
  once: bool,
  passive: bool,
};

module HtmlOrSvg = {
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
  type dynamicNodesPartItem('state, 'item) = {
    key: string,
    parts: dynamicParts('state, 'item),
  }
  and dynamicParts('state, 'item) = {
    target: option(Dom.node),
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
        iterator: 'state => list('item),
        keyFn: ('item, int) => string,
        dynamicsFn: ('item, int) => dynamicParts('state, 'item),
        start: Dom.comment,
        ending: Dom.comment,
        current: option(list(dynamicNodesPartItem('state, 'item))),
      })
    | DynamicSlotPart({
        el: Dom.element,
        func: 'state => component,
        ending: Dom.comment,
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
      })
    | DynamicEventPart({
        el: Dom.element,
        name: string,
        func: ('state, Dom.event) => unit,
      })
    | DynamicEventWithOptionsPart({
        el: Dom.element,
        name: string,
        func: ('state, Dom.event) => unit,
        eventOption,
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
              |> List.mapi((i, x) => {
                   let key = keyFn(x, i);
                   let item = current |> List.find_opt(x => x.key === key);

                   switch (item) {
                   | Some(item) =>
                     switch (item.parts.target) {
                     | Some(target) =>
                       parent |> Element.insertBefore(target, ending);
                       ();
                     | None => () // slot child
                     };

                     List.map(
                       updateDynamic(root, state, isSvg),
                       item.parts.dynamics,
                     );

                     item;
                   | None =>
                     let parts = dynamicsFn(x, i);

                     List.map(
                       updateDynamic(root, state, isSvg),
                       parts.dynamics,
                     );

                     {key, parts};
                   };
                 });

            // remove unneccesary nodes
            let firstItem = List.nth_opt(newList, 0);

            switch (firstItem) {
            | Some(firstItem) =>
              let firstItem = firstItem.parts.target;

              switch (firstItem) {
              | Some(firstItem) =>
                let rec remove = pre => {
                  switch (pre) {
                  | Some(pre) =>
                    if (pre !== (start |> Comment.asNode)) {
                      parent |> Element.removeChild(pre);
                      remove(firstItem |> Webapi.Dom.Node.previousSibling);
                    }

                  | None => ()
                  };
                };

                remove(firstItem |> Webapi.Dom.Node.previousSibling);
              | None => () // slot child
              };
            | None => ()
            };

            newList;
          | None =>
            iterable
            |> List.mapi((i, x) => {
                 let parts = dynamicsFn(x, i);

                 List.map(updateDynamic(el, state, isSvg), parts.dynamics);

                 {key: keyFn(x, i), parts};
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
      | DynamicSlotPart({el, func, ending}) =>
        let component = func(state);

        component((), el, isSvg, ~marker=Some(ending), ());
        DynamicSlotPart({el, func, ending});
      | DynamicStringAttributePart({el, name, func, current}) =>
        let value = func(state);
        switch (current) {
        | Some(current) =>
          if (value !== current) {
            HtmlOrSvg.setAttribute(el, name, value, isSvg);
          }
        | None => HtmlOrSvg.setAttribute(el, name, value, isSvg)
        };

        DynamicStringAttributePart({el, name, func, current: Some(value)});
      | DynamicBooleanAttributePart({el, name, func, current}) =>
        let value = func(state);

        let set = (el, name, value) =>
          if (value) {
            HtmlOrSvg.setAttribute(el, name, name, isSvg);
          } else {
            HtmlOrSvg.removeAttribute(el, name, isSvg);
          };

        switch (current) {
        | Some(current) =>
          if (value !== current) {
            set(el, name, value);
          }
        | None => set(el, name, value)
        };

        DynamicBooleanAttributePart({el, name, func, current: Some(value)});
      | DynamicEventPart({el, name, func}) =>
        DynamicEventPart({el, name, func})
      | DynamicEventWithOptionsPart({el, name, func, eventOption}) =>
        DynamicEventWithOptionsPart({el, name, func, eventOption})
      }
    );
  };
};

module CurrentStateCache = {
  type t;

  [@bs.new] external create: unit => t = "WeakMap";
  [@bs.send] external get: (t, Dom.element) => option('state) = "get";
  // [@bs.send] external has: (t, element) => bool = "has";
  // [@bs.send] external delete: (t, element) => unit = "delete";
  [@bs.send] external set: (t, Dom.element, 'state) => unit = "set";
};

let currentStateCache = CurrentStateCache.create();

type template('state, 'item) =
  list(DynamicPart.dynamicParts('state, 'item));

module TemplateCache = {
  type t;

  [@bs.new] external create: unit => t = "WeakMap";
  [@bs.send]
  external get: (t, Dom.element) => option(template('state, 'item)) = "get";
  // [@bs.send] external has: (t, Dom.element) => bool = "has";
  // [@bs.send] external delete: (t, Dom.element) => unit = "delete";
  [@bs.send]
  external set: (t, Dom.element, template('state, 'item)) => unit = "set";
};

let templateCache = TemplateCache.create();

module Attribute = {
  type attribute('state) =
    | DynamicAttribute(dynamicAttribute('state))
    | StaticAttribute(staticAttribute)
  and dynamicAttribute('state) =
    | DynamicStringAttribute(string, 'state => string)
    | DynamicBooleanAttribute(string, 'state => bool)
    | DynamicEvent(string, ('state, Dom.event) => unit)
    | DynamicEventWithOptions(
        string,
        ('state, Dom.event) => unit,
        eventOption,
      )
  and staticAttribute =
    | StaticStringAttribute(string, string)
    | StaticBooleanAttribute(string, bool)
    | StaticEvent(string, Dom.event => unit)
    | StaticEventWithOptions(string, Dom.event => unit, eventOption);

  open DynamicPart;

  let setAttribute =
      (
        root: Dom.element,
        attributes: list(attribute('state)),
        el: Dom.element,
        isSvg: bool,
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
             | DynamicEvent(name, func) =>
               el
               |> Element.addEventListener(name, e =>
                    switch (CurrentStateCache.get(currentStateCache, root)) {
                    | Some(state) => func(state, e)
                    | None => ()
                    }
                  );

               [DynamicEventPart({el, name, func})];
             | DynamicEventWithOptions(name, func, eventOption) =>
               el
               |> Element.addEventListenerWithOptions(
                    name,
                    e => {
                      switch (CurrentStateCache.get(currentStateCache, root)) {
                      | Some(state) => func(state, e)
                      | None => ()
                      }
                    },
                    {
                      "capture": eventOption.capture,
                      "once": eventOption.once,
                      "passive": eventOption.passive,
                    },
                  );
               [DynamicEventWithOptionsPart({el, name, func, eventOption})];
             }
           | StaticAttribute(attribute) =>
             switch (attribute) {
             | StaticStringAttribute(name, value) =>
               HtmlOrSvg.setAttribute(el, name, value, isSvg);
               [];
             | StaticBooleanAttribute(name, value) =>
               if (value) {
                 HtmlOrSvg.setAttribute(el, name, name, isSvg);
               } else {
                 HtmlOrSvg.removeAttribute(el, name, isSvg);
               };
               [];
             | StaticEvent(name, handler) =>
               el |> Element.addEventListener(name, handler);

               [];
             | StaticEventWithOptions(name, handler, eventOption) =>
               el
               |> Element.addEventListenerWithOptions(
                    name,
                    handler,
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

    {target: Some(el |> Element.asNode), dynamics};
  };
};

module Node = {
  open Attribute;

  type node('state, 'item) =
    | DynamicNode(dynamicNode('state, 'item))
    | StaticNode(staticNode('state, 'item))
  and dynamicNode('state, 'item) =
    | DynamicTextNode('state => string)
    | DynamicNodes(
        'state => list('item),
        ('item, int) => string,
        ('item, int) => node('state, 'item),
      )
    | DynamicSlot('state => component)
  and staticNode('state, 'item) =
    | StaticTextNode(string)
    | StaticElement(
        string,
        list(attribute('state)),
        list(node('state, 'item)),
        bool,
      )
    | StaticSlot(Dom.element => component);

  open Webapi.Dom;
  open DynamicPart;

  let createMarker = () => Document.createComment("", document);

  let rec append =
          (
            root: Dom.element,
            node: node('state, 'item),
            container: Dom.element,
            isSvg: bool,
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
          target: Some(text |> Text.asNode),
          dynamics: [
            DynamicTextNodePart({el: text |> Text.asNode, func, current: ""}),
          ],
        };
      | DynamicNodes(iterator, keyFn, childFn) => {
          target: None,
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
                  ~marker=Some(ending),
                  (),
                ),
              start,
              ending,
              current: None,
            }),
          ],
        }
      | DynamicSlot(func) => {
          target: None,
          dynamics: [DynamicSlotPart({el: container, func, ending})],
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

        {target: Some(text |> Text.asNode), dynamics: []};
      | StaticElement(name, attrs, nodes, isSvg) =>
        let el =
          container
          |> Element.insertBefore(
               HtmlOrSvg.createElement(name, isSvg),
               ending,
             );

        let marker = createMarker();
        el |> Element.appendChild(marker);

        let attrs = setAttribute(root, attrs, el, isSvg).dynamics;
        let children =
          List.flatten(
            nodes
            |> List.map(node =>
                 append(root, node, el, isSvg, ~marker=None, ()).dynamics
               ),
          );

        {
          target: Some(el |> Element.asNode),
          dynamics: List.flatten([attrs, children]),
        };
      | StaticSlot(func) =>
        let component = func(container);

        component((), container, isSvg, ~marker=Some(ending), ());

        {target: None, dynamics: []};
      }
    };
  };
};

module Template = {
  open DynamicPart;

  let define =
      (
        nodes: list(Node.node('state, 'item)),
        root: Dom.element,
        isSvg: bool,
        ~marker: option(Dom.comment),
        (),
      ) => {
    let cache = TemplateCache.get(templateCache, root);

    switch (cache) {
    | Some(_) => ()
    | None =>
      TemplateCache.set(
        templateCache,
        root,
        nodes
        |> List.map(node => Node.append(root, node, root, isSvg, ~marker, ())),
      )
    };
  };

  let update = (state: 'state, root: Dom.element, isSvg): unit => {
    let cache = TemplateCache.get(templateCache, root);

    CurrentStateCache.set(currentStateCache, root, state);

    switch (cache) {
    | Some(template) =>
      TemplateCache.set(
        templateCache,
        root,
        template
        |> List.map(parts => {
             {
               ...parts,
               dynamics:
                 parts.dynamics
                 |> List.map(DynamicPart.updateDynamic(root, state, isSvg)),
             }
           }),
      )
    | None => ()
    };
  };
};

let component =
    (
      factory: ('state => unit) => list(Node.node('state, 'item)),
      defaultState: 'state,
      (),
      el: Dom.element,
      isSvg: bool,
      ~marker: option(Dom.comment),
      (),
    ) => {
  let dipatch = state => Template.update(state, el, isSvg);

  Template.define(factory(dipatch), el, isSvg, ~marker, ());

  dipatch(defaultState);
};

let render = (component: component, el: Dom.element) =>
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
let slot = func => StaticNode(StaticSlot(func));
let slot_ = func => DynamicNode(DynamicSlot(func));

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
let event_ = (name, func) => DynamicAttribute(DynamicEvent(name, func));
let eventWithOptions = (name, handler, eventOption) =>
  StaticAttribute(StaticEventWithOptions(name, handler, eventOption));
let eventWithOptions_ = (name, func, eventOption) =>
  DynamicAttribute(DynamicEventWithOptions(name, func, eventOption));
