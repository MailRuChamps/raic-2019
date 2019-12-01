#include "CustomData.hpp"


CustomData::Log::Log() { }
CustomData::Log::Log(std::string text) : text(text) { }
CustomData::Log CustomData::Log::readFrom(InputStream& stream) {
    CustomData::Log result;
    result.text = stream.readString();
    return result;
}
void CustomData::Log::writeTo(OutputStream& stream) const {
    stream.write(TAG);
    stream.write(text);
}
std::string CustomData::Log::toString() const {
    return std::string("CustomData::Log") + "(" +
        text + 
        ")";
}

CustomData::Rect::Rect() { }
CustomData::Rect::Rect(Vec2Float pos, Vec2Float size, ColorFloat color) : pos(pos), size(size), color(color) { }
CustomData::Rect CustomData::Rect::readFrom(InputStream& stream) {
    CustomData::Rect result;
    result.pos = Vec2Float::readFrom(stream);
    result.size = Vec2Float::readFrom(stream);
    result.color = ColorFloat::readFrom(stream);
    return result;
}
void CustomData::Rect::writeTo(OutputStream& stream) const {
    stream.write(TAG);
    pos.writeTo(stream);
    size.writeTo(stream);
    color.writeTo(stream);
}
std::string CustomData::Rect::toString() const {
    return std::string("CustomData::Rect") + "(" +
        pos.toString() +
        size.toString() +
        color.toString() +
        ")";
}

CustomData::Line::Line() { }
CustomData::Line::Line(Vec2Float p1, Vec2Float p2, float width, ColorFloat color) : p1(p1), p2(p2), width(width), color(color) { }
CustomData::Line CustomData::Line::readFrom(InputStream& stream) {
    CustomData::Line result;
    result.p1 = Vec2Float::readFrom(stream);
    result.p2 = Vec2Float::readFrom(stream);
    result.width = stream.readFloat();
    result.color = ColorFloat::readFrom(stream);
    return result;
}
void CustomData::Line::writeTo(OutputStream& stream) const {
    stream.write(TAG);
    p1.writeTo(stream);
    p2.writeTo(stream);
    stream.write(width);
    color.writeTo(stream);
}
std::string CustomData::Line::toString() const {
    return std::string("CustomData::Line") + "(" +
        p1.toString() +
        p2.toString() +
        std::to_string(width) +
        color.toString() +
        ")";
}

CustomData::Polygon::Polygon() { }
CustomData::Polygon::Polygon(std::vector<ColoredVertex> vertices) : vertices(vertices) { }
CustomData::Polygon CustomData::Polygon::readFrom(InputStream& stream) {
    CustomData::Polygon result;
    result.vertices = std::vector<ColoredVertex>(stream.readInt());
    for (size_t i = 0; i < result.vertices.size(); i++) {
        result.vertices[i] = ColoredVertex::readFrom(stream);
    }
    return result;
}
void CustomData::Polygon::writeTo(OutputStream& stream) const {
    stream.write(TAG);
    stream.write((int)(vertices.size()));
    for (const ColoredVertex& verticesElement : vertices) {
        verticesElement.writeTo(stream);
    }
}
std::string CustomData::Polygon::toString() const {
    return std::string("CustomData::Polygon") + "(" +
        "TODO" + 
        ")";
}

CustomData::PlacedText::PlacedText() { }
CustomData::PlacedText::PlacedText(std::string text, Vec2Float pos, TextAlignment alignment, float size, ColorFloat color) : text(text), pos(pos), alignment(alignment), size(size), color(color) { }
CustomData::PlacedText CustomData::PlacedText::readFrom(InputStream& stream) {
    CustomData::PlacedText result;
    result.text = stream.readString();
    result.pos = Vec2Float::readFrom(stream);
    switch (stream.readInt()) {
    case 0:
        result.alignment = TextAlignment::LEFT;
        break;
    case 1:
        result.alignment = TextAlignment::CENTER;
        break;
    case 2:
        result.alignment = TextAlignment::RIGHT;
        break;
    default:
        throw std::runtime_error("Unexpected discriminant value");
    }
    result.size = stream.readFloat();
    result.color = ColorFloat::readFrom(stream);
    return result;
}
void CustomData::PlacedText::writeTo(OutputStream& stream) const {
    stream.write(TAG);
    stream.write(text);
    pos.writeTo(stream);
    stream.write((int)(alignment));
    stream.write(size);
    color.writeTo(stream);
}
std::string CustomData::PlacedText::toString() const {
    return std::string("CustomData::PlacedText") + "(" +
        text + 
        pos.toString() +
        "TODO" + 
        std::to_string(size) +
        color.toString() +
        ")";
}
std::shared_ptr<CustomData> CustomData::readFrom(InputStream& stream) {
    switch (stream.readInt()) {
    case 0:
        return std::shared_ptr<CustomData::Log>(new CustomData::Log(CustomData::Log::readFrom(stream)));
    case 1:
        return std::shared_ptr<CustomData::Rect>(new CustomData::Rect(CustomData::Rect::readFrom(stream)));
    case 2:
        return std::shared_ptr<CustomData::Line>(new CustomData::Line(CustomData::Line::readFrom(stream)));
    case 3:
        return std::shared_ptr<CustomData::Polygon>(new CustomData::Polygon(CustomData::Polygon::readFrom(stream)));
    case 4:
        return std::shared_ptr<CustomData::PlacedText>(new CustomData::PlacedText(CustomData::PlacedText::readFrom(stream)));
    default:
        throw std::runtime_error("Unexpected discriminant value");
    }
};
