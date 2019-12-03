namespace AiCup2019

type Debug(writer) =
    member this.draw(customData) =
        let message : Model.PlayerMessageGameCustomDataMessage = {Data = customData}
        message.writeTo writer
        writer.Flush()
