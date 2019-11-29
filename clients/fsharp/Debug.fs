namespace AiCup2019

type Debug(writer) =
    member this.draw(customData) =
        let message : Model.CustomDataMessage = {CustomData = customData}
        message.writeTo writer
        writer.Flush()
