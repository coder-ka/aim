open Jest;
open Webapi.Dom;

module ChildComponent = {
  let createElement = (~el, ~defaultState, ~children, ()) => {
    open Aim;

    let span = html("span");

    define(span([text_(state => state)]), el);

    update(defaultState, el);
  };
};

module Counter = {
  let increment = count => count + 1;

  let createElement = (~el, ~children, ()) => {
    open Aim;

    let div = html("div");
    let button = html("button");
    let span = html("span");

    define(
      div([
        span([
          attr("id", "display"),
          text_(count => Js.Int.toString(count)),
        ]),
        button([
          attr("id", "inc"),
          event_("click", (_, count) =>
            update(increment(count), el)
          ),
          text("+"),
          slot_((el, count) =>
            <ChildComponent el={el} defaultState={Js.Int.toString(count)} />
          ),
        ]),
        nodes_(
          count =>
            [1, 2, 3, 4, 5, 6]
            |> List.filter(x => x > count)
            |> List.sort((cur, next) =>
                 count mod 2 === 0 ? cur - next : next - cur
               ),
          (num, _) => Js.Int.toString(num),
          (num, _) =>
            span([
              attr("count", Js.Int.toString(num)),
              text_(count => Js.Int.toString((count + 1) * num)),
            ]),
        ),
      ]),
      el,
    );

    update(0, el);
  };
};

describe("Expect", () => {
  open Expect;
  test("increment", () =>
    expect(Counter.increment(1)) |> toBe(2)
  );

  test("click increment button", () => {
    let body = Document.querySelector("body", document);
    switch (body) {
    | Some(body) =>
      let container = Document.createElement("div", document);
      body |> Element.appendChild(container);
      container |> Element.setAttribute("id", "app");

      // render component
      let container = document |> Document.getElementById("app");
      let container =
        switch (container) {
        | Some(root) => root
        | None =>
          exception RootElement_NotFound;
          raise(RootElement_NotFound);
        };

      <Counter el={container}/>

      // click +
      let incButton = container |> Element.querySelector("button#inc");
      switch (incButton) {
      | Some(el) =>
        Element.dispatchEvent(MouseEvent.make("click"), el);
        ();
      | None =>
        exception IncrementButton_NotFound;
        raise(IncrementButton_NotFound);
      };
      // get display count
      let count =
        switch (Element.querySelector("#display", container)) {
        | Some(el) => Element.textContent(el)
        | None => ""
        };

      expect(count) |> toBe("1");
    | None =>
      exception Body_NotFound;
      raise(Body_NotFound);
    };
  });
});
