const Vec2Float = require('./vec2float').Vec2Float;
const ColorFloat = require('./color-float').ColorFloat;
const ColoredVertex = require('./colored-vertex').ColoredVertex;

class CustomData {
    static async readFrom (stream) {
        const discriminant = await stream.readInt();
        if (discriminant === Log.TAG) {
            return await CustomData.Log.readFrom(stream);
        }
        if (discriminant === Rect.TAG) {
            return await CustomData.Rect.readFrom(stream);
        }
        if (discriminant === Line.TAG) {
            return await CustomData.Line.readFrom(stream);
        }
        if (discriminant === Polygon.TAG) {
            return await CustomData.Polygon.readFrom(stream);
        }
        throw new Error('Unexpected discriminant value');
    }
}

class Log extends CustomData {
    constructor (text) {
        super();
        this.text = text;
    }
    
    static async readFrom (stream) {
        const text = await stream.readString();
        return new Log(text);
    }

    async writeTo (stream) {
        await stream.writeInt(this.TAG);
        await stream.writeString(this.text);
    }

    toString () {
        return 'Log(' +
            this.text +
            ')';
    }
}
Log.TAG = 0;

class Rect extends CustomData {
    constructor (pos, size, color) {
        super();
        this.pos = pos;
        this.size = size;
        this.color = color;
    }

    static async readFrom (stream) {
        const pos = await Vec2Float.readFrom(stream);
        const size = await Vec2Float.readFrom(stream);
        const color = await ColorFloat.readFrom(stream);
        return new Rect(pos, size, color);
    }
    
    async writeTo (stream) {
        await stream.writeInt(this.TAG);
        await this.pos.writeTo(stream);
        await this.size.writeTo(stream);
        await this.color.writeTo(stream);
    }

    toString () {
        return 'Rect(' +
            this.pos + ',' +
            this.size + ',' +
            this.color +
            ')';
    }
}
Rect.TAG = 1;

class Line extends CustomData {
    constructor (p1, p2, width, color) {
        super();
        this.p1 = p1;
        this.p2 = p2;
        this.width = width;
        this.color = color;
    }

    static async readFrom (stream) {
        const p1 = await Vec2Float.readFrom(stream);
        const p2 = await Vec2Float.readFrom(stream);
        const width = await stream.readFloat();
        const color = await ColorFloat.readFrom(stream);
        return new Line(p1, p2, width, color);
    }
    
    async writeTo (stream) {
        await stream.writeInt(this.TAG);
        await this.p1.writeTo(stream);
        await this.p2.writeTo(stream);
        await stream.writeFloat(this.width);
        await this.color.writeTo(stream);
    }

    toString () {
        return 'Line(' +
            this.p1 + ',' +
            this.p2 + ',' +
            this.width + ',' +
            this.color +
            ')';
    }
}
Line.TAG = 2;

class Polygon extends CustomData {
    constructor (vertices) {
        super();
        this.vertices = vertices;
    }
    static async readFrom (stream) {
        const vertices = [];
        for (let i = 0, verticesSize = await stream.readInt(); i < verticesSize; i++) {
            const vertex = await ColoredVertex.readFrom(stream);
            vertices.push(vertex);
        }
        return new Polygon(vertices);
    }

    async writeTo (stream) {
        await stream.writeInt(this.TAG);
        await stream.writeInt(this.vertices.length);
        for (let i = 0, verticesSize = this.vertices.length; i < verticesSize; i++) {
            await this.vertices[i].writeTo(stream);
        }
    }

    toString () {
        return 'Polygon(' +
            this.vertices +
            ')';
    }
}
Polygon.TAG = 3;


module.exports = {
    CustomData: CustomData,
    Log: Log,
    Rect: Rect,
    Line: Line,
    Polygon: Polygon
};
