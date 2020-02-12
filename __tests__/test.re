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

    let initialState = 0;
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

module ChildComponent = {
  open Aim;
  open Elements;

  let createElement = (~children, ()) => {
    component(
      <> <span> {text("hoge")} </span> </>,
      0,
      (_, _, _) => {()},
      update => {
        CountStore.subscribe(update);
        ();
      },
    );
  };
};

let generateArray = () => {
  let arr = Array.make(100, 0) |> Array.map(_ => Js.Math.random_int(0, 999));
  arr;
};
let sampled = generateArray();

module Counter = {
  open Aim;
  open Elements;

  let increment = count => count + 1;

  type mutations =
    | Increment;

  let createElement = (~children, ()) => {
    let children = children |> List.map(slot);

    component(
      <>
        <div>
          <span id="display"> {text_(count => Js.Int.toString(count))} </span>
          <button id="inc" onClick={(_, dispatch) => dispatch(Increment)}>
            {text("+")}
          </button>
          <div> ...children </div>
        </div>
        {nodes_(
           count => {
             let filtered =
               [|1, 2, 3, 4, 5|] |> Js.Array.filter(x => x > count);
             filtered
             |> Array.sort((cur, next) =>
                  count mod 2 === 0 ? cur - next : next - cur
                );
             filtered;
           },
           (_, i) => Js.Int.toString(i),
           (num, _) =>
             <span id={Js.Int.toString(num)}>
               {text_(count => Js.Int.toString((count + 1) * num))}
             </span>,
         )}
      </>,
      0,
      (update, mutation, count) => {
        switch (mutation) {
        | Increment => CountStore.dispatch(CountStore.Increment)
        }
      },
      update => {
        CountStore.subscribe(update);
        ();
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

      Aim.render(<Counter> <ChildComponent /> </Counter>, container);

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
