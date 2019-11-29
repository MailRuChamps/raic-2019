#ifndef _MODEL_CUSTOM_DATA_HPP_
#define _MODEL_CUSTOM_DATA_HPP_

#include "../Stream.hpp"
#include <memory>
#include <string>
#include <stdexcept>
#include <stdexcept>
#include "Vec2Float.hpp"
#include <stdexcept>
#include "Vec2Float.hpp"
#include <stdexcept>
#include "ColorFloat.hpp"
#include <stdexcept>
#include "Vec2Float.hpp"
#include <stdexcept>
#include "Vec2Float.hpp"
#include <stdexcept>
#include "ColorFloat.hpp"
#include <vector>
#include <stdexcept>
#include "ColoredVertex.hpp"
#include <stdexcept>
#include "Vec2Float.hpp"
#include <stdexcept>
#include "ColorFloat.hpp"
#include <stdexcept>
#include "Vec2Float.hpp"
#include <stdexcept>
#include "TextAlignment.hpp"
#include <stdexcept>
#include "ColorFloat.hpp"

class CustomData {
public:
    class Log;
    class Rect;
    class Line;
    class Polygon;
    class PlacedText;

    static std::shared_ptr<CustomData> readFrom(InputStream& stream);
    virtual void writeTo(OutputStream& stream) const = 0;
    virtual std::string toString() const = 0;
};

class CustomData::Log : public CustomData {
public:
    static const int TAG = 0;
public:
    std::string text;
    Log();
    Log(std::string text);
    static Log readFrom(InputStream& stream);
    void writeTo(OutputStream& stream) const;
    std::string toString() const override;
};

class CustomData::Rect : public CustomData {
public:
    static const int TAG = 1;
public:
    Vec2Float pos;
    Vec2Float size;
    ColorFloat color;
    Rect();
    Rect(Vec2Float pos, Vec2Float size, ColorFloat color);
    static Rect readFrom(InputStream& stream);
    void writeTo(OutputStream& stream) const;
    std::string toString() const override;
};

class CustomData::Line : public CustomData {
public:
    static const int TAG = 2;
public:
    Vec2Float p1;
    Vec2Float p2;
    float width;
    ColorFloat color;
    Line();
    Line(Vec2Float p1, Vec2Float p2, float width, ColorFloat color);
    static Line readFrom(InputStream& stream);
    void writeTo(OutputStream& stream) const;
    std::string toString() const override;
};

class CustomData::Polygon : public CustomData {
public:
    static const int TAG = 3;
public:
    std::vector<ColoredVertex> vertices;
    Polygon();
    Polygon(std::vector<ColoredVertex> vertices);
    static Polygon readFrom(InputStream& stream);
    void writeTo(OutputStream& stream) const;
    std::string toString() const override;
};

class CustomData::PlacedText : public CustomData {
public:
    static const int TAG = 4;
public:
    std::string text;
    Vec2Float pos;
    TextAlignment alignment;
    float size;
    ColorFloat color;
    PlacedText();
    PlacedText(std::string text, Vec2Float pos, TextAlignment alignment, float size, ColorFloat color);
    static PlacedText readFrom(InputStream& stream);
    void writeTo(OutputStream& stream) const;
    std::string toString() const override;
};

#endif
