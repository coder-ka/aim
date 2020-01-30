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

type template('state, 'item) = {
  root: domElement,
  parts: parts('state, 'item),
};

let define: (child('state, 'item), domElement) => unit;

let update: ('state, domElement) => unit;

let html: (string, list(child('state, 'item))) => child('state, 'item);
let svg: (string, list(child('state, 'item))) => child('state, 'item);

let text: string => child('state, 'item);
let text_: ('state => string) => child('state, 'item);
let attr: (string, string) => child('state, 'item);
let attr_: (string, 'state => string) => child('state, 'item);
let boolAttr: (string, bool) => child('state, 'item);
let boolAttr_: (string, 'state => bool) => child('state, 'item);
let event: (string, domEvent => unit) => child('state, 'item);
let eventWithOptions:
  (string, domEvent => unit, domEventOption) => child('state, 'item);
let event_: (string, (domEvent, 'state) => unit) => child('state, 'item);
let eventWithOptions_:
  (string, (domEvent, 'state) => unit, domEventOption) => child('state, 'item);
let slot: (domElement => unit) => child('state, 'item);
let slot_: ((domElement, 'state) => unit) => child('state, 'item);
let nodes_:
  (
    'state => list('item),
    ('item, int) => string,
    ('item, int) => child('state, 'item)
  ) =>
  child('state, 'item);
