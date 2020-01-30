open Webapi.Dom;

type domNode = Dom.node;
type domElement = Dom.element;
type domText = Dom.text;
type domComment = Dom.comment;
type domEvent = Dom.event;
type domEventOption = {
  capture: bool,
  once: bool,
  passive: bool,
};

type element =
  | HtmlElement(domElement)
  | SvgElement(domElement);

type parts('state, 'item) = {
  root: option(domNode),
  parts: list(part('state, 'item)),
}
and keyedParts('state, 'item) = {
  key: string,
  parts: parts('state, 'item),
}
and part('state, 'item) =
  | TextPart({
      target: domText,
      func: 'state => string,
      current: option(string),
    })
  | AttributePart({
      target: element,
      func: 'state => string,
      name: string,
      current: option(string),
    })
  | BooleanAttributePart({
      target: element,
      func: 'state => bool,
      name: string,
      current: option(bool),
    })
  | EventPart({target: element})
  | EventWithOptionsPart({target: element})
  | SlotPart({
      target: element,
      func: (domElement, 'state) => unit,
    })
  | NodesPart({
      target: element,
      iteratorFn: 'state => list('item),
      keyFn: ('item, int) => string,
      childFn: ('item, int) => child('state, 'item),
      start: domComment,
      ending: domComment,
      current: option(list(keyedParts('state, 'item))),
    })
and nodeChild('state, 'item) =
  | TextChild(string)
  | TextPartChild('state => string)
  | NodeChild(string, list(child('state, 'item)), bool)
  | NodesPartChild(
      'state => list('item),
      ('item, int) => string,
      ('item, int) => child('state, 'item),
    )
and slotChild('state) =
  | SlotChild(domElement => unit)
  | SlotPartChild((domElement, 'state) => unit)
and attributeChild('state) =
  | AttributeChild(string, string)
  | BooleanAttributeChild(string, bool)
  | EventChild(string, domEvent => unit)
  | EventWithOptionsChild(string, domEvent => unit, domEventOption)
  | AttributePartChild(string, 'state => string)
  | BooleanAttributePartChild(string, 'state => bool)
  | EventPartChild(string, (domEvent, 'state) => unit)
  | EventWithOptionsPartChild(
      string,
      (domEvent, 'state) => unit,
      domEventOption,
    )
and child('state, 'item) =
  | Node(nodeChild('state, 'item))
  | Slot(slotChild('state))
  | Attribute(attributeChild('state));

let svgNameSpace = "http://www.w3.org/2000/svg";
let setAttribute = (name: string, value: string, element: element) => {
  switch (element) {
  | HtmlElement(element) => element |> Element.setAttribute(name, value)
  | SvgElement(element) =>
    element |> Element.setAttributeNS(svgNameSpace, name, value)
  };
};
let removeAttribute = (name: string, element: element) => {
  switch (element) {
  | HtmlElement(element) => element |> Element.removeAttribute(name)
  | SvgElement(element) =>
    element |> Element.removeAttributeNS(svgNameSpace, name)
  };
};
let createElement = name => {
  HtmlElement(document |> Document.createElement(name));
};
let createSvgElement = name => {
  SvgElement(document |> Document.createElementNS(svgNameSpace, name));
};

module CurrentStateCache = {
  type t;

  [@bs.new] external create: unit => t = "WeakMap";
  [@bs.send] external get: (t, element) => option('state) = "get";
  // [@bs.send] external has: (t, element) => bool = "has";
  // [@bs.send] external delete: (t, element) => unit = "delete";
  [@bs.send] external set: (t, element, 'state) => unit = "set";
};

let csCache = CurrentStateCache.create();

let createMarker = () => document |> Document.createComment("");

let rec createParts = (child, el, start, ending) => {
  let parent =
    switch (el) {
    | HtmlElement(el) => el
    | SvgElement(el) => el
    };

  switch (child) {
  | Node(child) =>
    switch (child) {
    | TextChild(str) =>
      let text = document |> Document.createTextNode(str);
      parent |> Element.insertBefore(text, ending);

      {root: Some(text |> Text.asNode), parts: []};
    | TextPartChild(func) =>
      let text = document |> Document.createTextNode("");

      parent |> Element.insertBefore(text, ending);

      {
        root: Some(text |> Text.asNode),
        parts: [TextPart({target: text, func, current: None})],
      };
    | NodeChild(name, children, isSvg) =>
      let childElem = isSvg ? createSvgElement(name) : createElement(name);

      let childEl =
        switch (childElem) {
        | HtmlElement(childEl) => childEl
        | SvgElement(childEl) => childEl
        };

      parent |> Element.insertBefore(childEl, ending);

      let start = createMarker();
      childEl |> Element.appendChild(start);
      let ending = createMarker();
      childEl |> Element.appendChild(ending);

      {
        root: Some(childEl |> Element.asNode),
        parts:
          children
          |> List.map(child =>
               createParts(child, childElem, start, ending).parts
             )
          |> List.flatten,
      };
    | NodesPartChild(iteratorFn, keyFn, childFn) =>
      let start = createMarker();
      let ending = createMarker();
      parent |> Element.appendChild(start);
      parent |> Element.appendChild(ending);

      {
        root: None,
        parts: [
          NodesPart({
            target: el,
            iteratorFn,
            keyFn,
            childFn,
            start,
            ending,
            current: None,
          }),
        ],
      };
    }
  | Slot(child) =>
    switch (child) {
    | SlotChild(func) =>
      func(parent);

      {root: None, parts: []};
    | SlotPartChild(func) => {
        root: None,
        parts: [SlotPart({target: el, func})],
      }
    }
  | Attribute(child) =>
    switch (child) {
    | AttributeChild(name, value) =>
      el |> setAttribute(name, value);

      {root: None, parts: []};
    | AttributePartChild(name, func) => {
        root: None,
        parts: [AttributePart({target: el, func, name, current: None})],
      }
    | BooleanAttributeChild(name, value) =>
      if (value) {
        el |> setAttribute(name, name);
      };

      {root: None, parts: []};
    | BooleanAttributePartChild(name, func) => {
        root: None,
        parts: [
          BooleanAttributePart({target: el, func, name, current: None}),
        ],
      }
    | EventChild(name, handler) =>
      parent |> Element.addEventListener(name, handler);

      {root: None, parts: []};
    | EventPartChild(name, func) =>
      parent
      |> Element.addEventListener(
           name,
           e => {
             let currentState = CurrentStateCache.get(csCache, el);
             switch (currentState) {
             | Some(currentState) => func(e, currentState)
             | None => ()
             };
           },
         );

      {root: None, parts: [EventPart({target: el})]};
    | EventWithOptionsChild(name, handler, option) =>
      parent
      |> Element.addEventListenerWithOptions(
           name,
           handler,
           {
             "capture": option.capture,
             "once": option.once,
             "passive": option.passive,
           },
         );

      {root: None, parts: []};
    | EventWithOptionsPartChild(name, func, option) =>
      parent
      |> Element.addEventListenerWithOptions(
           name,
           e => {
             let currentState = CurrentStateCache.get(csCache, el);
             switch (currentState) {
             | Some(currentState) => func(e, currentState)
             | None => ()
             };
           },
           {
             "capture": option.capture,
             "once": option.once,
             "passive": option.passive,
           },
         );

      {root: None, parts: [EventWithOptionsPart({target: el})]};
    }
  };
}
and updateParts = (state, partList) => {
  let parts =
    partList.parts
    |> List.map(part =>
         switch (part) {
         | TextPart({target, func, current}) =>
           let next = func(state);
           switch (current) {
           | Some(current) =>
             next != current ? target->Text.setTextContent(next) : ()
           | None => target->Text.setTextContent(next)
           };

           TextPart({target, func, current: Some(next)});
         | AttributePart({target, func, name, current}) =>
           let next = func(state);

           switch (current) {
           | Some(current) =>
             next != current ? target |> setAttribute(name, next) : ()
           | None => target |> setAttribute(name, next)
           };

           AttributePart({target, func, name, current: Some(next)});
         | BooleanAttributePart({target, func, name, current}) =>
           let next = func(state);

           switch (current) {
           | Some(current) =>
             next != current
               ? target
                 |> (next ? setAttribute(name, name) : removeAttribute(name))
               : ()
           | None =>
             target
             |> (next ? setAttribute(name, name) : removeAttribute(name))
           };

           BooleanAttributePart({target, func, name, current: Some(next)});
         | EventPart(x) =>
           CurrentStateCache.set(csCache, x.target, state);

           EventPart(x);
         | EventWithOptionsPart(x) =>
           CurrentStateCache.set(csCache, x.target, state);

           EventWithOptionsPart(x);
         | SlotPart({target, func}) =>
           let el =
             switch (target) {
             | HtmlElement(el) => el
             | SvgElement(el) => el
             };

           func(el, state);

           SlotPart({target, func});
         | NodesPart({
             target,
             iteratorFn,
             keyFn,
             childFn,
             start,
             ending,
             current,
           }) =>
           let parent =
             switch (target) {
             | HtmlElement(target) => target
             | SvgElement(target) => target
             };

           let iterable = iteratorFn(state);

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
                        let el = item.parts.root;
                        switch (el) {
                        | Some(el) =>
                          parent |> Element.insertBefore(el, ending);
                          updateParts(state, item.parts);
                          item;
                        | None => item
                        };
                      | None =>
                        let parts =
                          createParts(childFn(x, i), target, start, ending);
                        updateParts(state, parts);
                        {key, parts};
                      };
                    });

               // remove unneccesary nodes
               let first = List.nth_opt(newList, 0);
               switch (first) {
               | Some(first) =>
                 switch (first.parts.root) {
                 | Some(el) =>
                   let rec remove = pre => {
                     switch (pre) {
                     | Some(pre) =>
                       if (pre !== (start |> Comment.asNode)) {
                         parent |> Element.removeChild(pre);
                         remove(el |> Webapi.Dom.Node.previousSibling);
                       }
                     | None => ()
                     };
                   };
                   remove(el |> Webapi.Dom.Node.previousSibling);
                 | None => ()
                 }
               | None => ()
               };

               //
               newList;
             | None =>
               iterable
               |> List.mapi((i, x) => {
                    let parts =
                      createParts(childFn(x, i), target, start, ending);
                    updateParts(state, parts);
                    {key: keyFn(x, i), parts};
                  })
             };
           NodesPart({
             target,
             iteratorFn,
             keyFn,
             childFn,
             start,
             ending,
             current: Some(current),
           });
         }
       );

  {root: partList.root, parts};
};

type template('state, 'item) = {
  root: Dom.element,
  parts: parts('state, 'item),
};

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

let cache = TemplateCache.create();

let update = (state: 'state, container: Dom.element): unit => {
  let template = TemplateCache.get(cache, container);

  switch (template) {
  | Some(template) =>
    TemplateCache.set(
      cache,
      container,
      {root: template.root, parts: updateParts(state, template.parts)},
    );
    ();
  | None =>
    exception NoTemplateCached;
    raise(NoTemplateCached);
  };
};

let define = (child, container) => {
  let templateCache = TemplateCache.get(cache, container);

  let template =
    switch (templateCache) {
    | Some(templateCache) => templateCache
    | None =>
      let start = createMarker();
      container |> Element.appendChild(start);

      let ending = createMarker();
      container |> Element.appendChild(ending);

      {
        root: container,
        parts: createParts(child, HtmlElement(container), start, ending),
      };
    };

  TemplateCache.set(cache, container, template);

  ();
};

let html = (name, children) => Node(NodeChild(name, children, false));
let svg = (name, children) => Node(NodeChild(name, children, true));

let text = str => Node(TextChild(str));
let text_ = func => Node(TextPartChild(func));

let nodes_ = (iteratorFn, keyFn, templateFn) =>
  Node(NodesPartChild(iteratorFn, keyFn, templateFn));

let slot = func => Slot(SlotChild(func));
let slot_ = func => Slot(SlotPartChild(func));

let attr = (name, value) => Attribute(AttributeChild(name, value));
let attr_ = (name, func) => Attribute(AttributePartChild(name, func));

let boolAttr = (name, value) =>
  Attribute(BooleanAttributeChild(name, value));
let boolAttr_ = (name, func) =>
  Attribute(BooleanAttributePartChild(name, func));

let event = (name, handler) => Attribute(EventChild(name, handler));
let event_ = (name, func) => Attribute(EventPartChild(name, func));
let eventWithOptions = (name, handler, option) =>
  Attribute(EventWithOptionsChild(name, handler, option));
let eventWithOptions_ = (name, func, option) =>
  Attribute(EventWithOptionsPartChild(name, func, option));
