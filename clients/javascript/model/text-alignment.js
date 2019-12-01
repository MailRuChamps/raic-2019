class TextAlignment {
    constructor (discriminant) {
        this.discriminant = discriminant;
    }

    static async readFrom (stream) {
        switch (await stream.readInt()) {
        case 0:
            return new TextAlignment(TextAlignment.LEFT);
        case 1:
            return new TextAlignment(TextAlignment.CENTER);
        case 2:
            return new TextAlignment(TextAlignment.RIGHT);
        default:
            throw new Error('Unexpected discriminant value');
        }
    }
}

TextAlignment.LEFT = 0;
TextAlignment.CENTER = 1;
TextAlignment.RIGHT = 2;

module.exports.TextAlignment = TextAlignment;
