namespace AiCup2019

module Debug =
    type T(writer) =
        member this.draw(customData) =
           (new Model.CustomDataMessage.T(customData) :> Model.PlayerMessageGame.T).writeTo writer
           writer.Flush()

