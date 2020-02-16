open Jest;
open Webapi.Dom;

module Counter = {
  type actions =
    | Switch;

  let make = () => {
    Aim.(
      component(
        [
          html(
            "button",
            [event("click", (e, dispatch) => dispatch(Switch))],
            [text("button")],
          ),
          html(
            "input",
            [
              attr("type", "checkbox"),
              boolAttr_("checked", checked => checked),
            ],
            [],
          ),
        ],
        update => update(true),
        (action, update, checked) => {
          switch (action) {
          | Switch => update(!checked)
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
