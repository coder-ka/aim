open Jest;
open Webapi.Dom;

module Elements = {
  open Aim;

  let div = (~children, ()) => html("div", [], children);
  let button = (~id, ~onClick, ~children, ()) =>
    html("button", [attr("id", id), event("click", onClick)], children);

  let span = (~id="", ~children, ()) =>
    html("span", [attr("id", id)], children);
};

module CountStore = {
  module State = {
    type state = int;

    let init = () => 0;
  };

  include Aim.Store(State);

  type actions =
    | Increment;

  let dispatch = action => {
    switch (action) {
    | Increment => commit(current^ + 1)
    };
  };
};

module Counter = {
  open Aim;
  open Elements;

  let increment = count => count + 1;

  type actions =
    | Increment;

  let createElement = (~children, ()) => {
    component(
      <>
        <div>
          <span id="display"> {text_(count => Js.Int.toString(count))} </span>
          <button id="inc" onClick={(_, dispatch) => dispatch(Increment)}>
            {text("+")}
          </button>
        </div>
      </>,
      update => {
        // initialize by initialized store state
        update(CountStore.current^);

        // subscribe
        CountStore.subscribe(update);
      },
      (action, _, _) => {
        switch (action) {
        | Increment => CountStore.dispatch(CountStore.Increment)
        }
      },
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
        Element.dispatchEvent(MouseEvent.make("click"), el) ? () : ()
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
