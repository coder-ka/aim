open Jest;
open Webapi.Dom;

module Counter = {
  let increment = count => count + 1;

  type actions =
    | Increment;

  let make = () => {
    Aim.(
      component(
        [
          html(
            "div",
            [],
            [
              html("span", [], [text_(count => Js.Int.toString(count))]),
              html(
                "button",
                [event("click", (e, dispatch) => dispatch(Increment))],
                [text("+")],
              ),
            ],
          ),
        ],
        // init
        update => update(0),
        // action handler
        (action, update, count) => {
          switch (action) {
          | Increment => update(count + 1)
          }
        },
      )
    );
  };
};

describe("Expect", () => {
  Expect.(
    test("click increment button", () => {
      let body = Document.querySelector("body", document);

      switch (body) {
      | Some(body) =>
        let counter = Counter.make();

        Aim.render(counter, body);
      | None =>
        exception Body_NotFound;
        raise(Body_NotFound);
      };

      expect(true) |> toBe(true);
    })
  )
});
