const Vec2Float = require('./vec2-float').Vec2Float;
const ColorFloat = require('./color-float').ColorFloat;
const ColoredVertex = require('./colored-vertex').ColoredVertex;
class CustomData {
    static async readFrom(stream) {
        let discriminant = await stream.readInt();
        if (discriminant == Log.TAG) {
            return await Log.readFrom(stream);
        }
        if (discriminant == Rect.TAG) {
            return await Rect.readFrom(stream);
        }
        if (discriminant == Line.TAG) {
            return await Line.readFrom(stream);
        }
        if (discriminant == Polygon.TAG) {
            return await Polygon.readFrom(stream);
        }
        if (discriminant == PlacedText.TAG) {
            return await PlacedText.readFrom(stream);
        }
        throw new Error("Unexpected discriminant value");
    }
}

class Log extends CustomData {
    constructor(text) {
        super();
        this.text = text;
    }
    static async readFrom(stream) {
        let text;
        text = await stream.readString();
        return new Log(text);
    }
    async writeTo(stream) {
        await stream.writeInt(Log.TAG);
        let text = this.text;
        await stream.writeString(text);
    }
}
CustomData.Log = Log;
Log.TAG = 0;
class Rect extends CustomData {
    constructor(pos, size, color) {
        super();
        this.pos = pos;
        this.size = size;
        this.color = color;
    }
    static async readFrom(stream) {
        let pos;
        pos = await Vec2Float.readFrom(stream);
        let size;
        size = await Vec2Float.readFrom(stream);
        let color;
        color = await ColorFloat.readFrom(stream);
        return new Rect(pos, size, color);
    }
    async writeTo(stream) {
        await stream.writeInt(Rect.TAG);
        let pos = this.pos;
        await pos.writeTo(stream);
        let size = this.size;
        await size.writeTo(stream);
        let color = this.color;
        await color.writeTo(stream);
    }
}
CustomData.Rect = Rect;
Rect.TAG = 1;
class Line extends CustomData {
    constructor(p1, p2, width, color) {
        super();
        this.p1 = p1;
        this.p2 = p2;
        this.width = width;
        this.color = color;
    }
    static async readFrom(stream) {
        let p1;
        p1 = await Vec2Float.readFrom(stream);
        let p2;
        p2 = await Vec2Float.readFrom(stream);
        let width;
        width = await stream.readFloat();
        let color;
        color = await ColorFloat.readFrom(stream);
        return new Line(p1, p2, width, color);
    }
    async writeTo(stream) {
        await stream.writeInt(Line.TAG);
        let p1 = this.p1;
        await p1.writeTo(stream);
        let p2 = this.p2;
        await p2.writeTo(stream);
        let width = this.width;
        await stream.writeFloat(width);
        let color = this.color;
        await color.writeTo(stream);
    }
}
CustomData.Line = Line;
Line.TAG = 2;
class Polygon extends CustomData {
    constructor(vertices) {
        super();
        this.vertices = vertices;
    }
    static async readFrom(stream) {
        let vertices;
        vertices = [];
        for (let i = await stream.readInt(); i > 0; i--) {
            let verticesElement;
            verticesElement = await ColoredVertex.readFrom(stream);
            vertices.push(verticesElement);
        }
        return new Polygon(vertices);
    }
    async writeTo(stream) {
        await stream.writeInt(Polygon.TAG);
        let vertices = this.vertices;
        await stream.writeInt(vertices.length);
        for (let verticesElement of vertices) {
            await verticesElement.writeTo(stream);
        }
    }
}
CustomData.Polygon = Polygon;
Polygon.TAG = 3;
class PlacedText extends CustomData {
    constructor(text, pos, alignment, size, color) {
        super();
        this.text = text;
        this.pos = pos;
        this.alignment = alignment;
        this.size = size;
        this.color = color;
    }
    static async readFrom(stream) {
        let text;
        text = await stream.readString();
        let pos;
        pos = await Vec2Float.readFrom(stream);
        let alignment;
        alignment = await stream.readInt();
        let size;
        size = await stream.readFloat();
        let color;
        color = await ColorFloat.readFrom(stream);
        return new PlacedText(text, pos, alignment, size, color);
    }
    async writeTo(stream) {
        await stream.writeInt(PlacedText.TAG);
        let text = this.text;
        await stream.writeString(text);
        let pos = this.pos;
        await pos.writeTo(stream);
        let alignment = this.alignment;
        await stream.writeInt(alignment);
        let size = this.size;
        await stream.writeFloat(size);
        let color = this.color;
        await color.writeTo(stream);
    }
}
CustomData.PlacedText = PlacedText;
PlacedText.TAG = 4;
module.exports = { CustomData: CustomData }
