open Jest;
open Webapi.Dom;

module Counter = {
  let increment = count => count + 1;

  let createElement = (~children, ()) => {
    open Aim;

    let div = h("div");

    let button = (~id, ~onClick=_ => (), ~children, ()) =>
      h(
        "button",
        ~attrs=[attr("id", id), event("click", onClick)],
        ~children,
        (),
      );

    let span = (~id, ~children, ()) =>
      h("span", ~attrs=[attr("id", id)], ~children, ());

    component(
      (state, update) =>
        <div>
          <span id="display"> {text(Js.Int.toString(state))} </span>
          <button id="inc" onClick={_ => update(increment(state))}>
            {text("+")}
          </button>
        </div>,
      0,
    );
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

      Aim.render(<Counter />, container);

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
