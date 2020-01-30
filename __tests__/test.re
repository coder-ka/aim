open Jest;
open Webapi.Dom;

module ChildComponent = {
  let start = (container, defaultState) => {
    open Aim;

    let span = html("span");

    define(span([text_(state => state)]), container);

    update(defaultState, container);
  };
};

module Counter = {
  let increment = count => count + 1;

  let start = (container, style) => {
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
          style,
          event_("click", (_, count) =>
            update(increment(count), container)
          ),
          text("+"),
          slot_((el, count) =>
            ChildComponent.start(el, Js.Int.toString(count))
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
      container,
    );

    update(0, container);
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

      Counter.start(container, Aim.attr("class", "bg-primary-500"));

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
