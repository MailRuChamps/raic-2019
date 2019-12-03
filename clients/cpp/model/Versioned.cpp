#include "Versioned.hpp"

Versioned::Versioned() { }
Versioned::Versioned(std::unordered_map<int, UnitAction> inner) : inner(inner) { }
Versioned Versioned::readFrom(InputStream& stream) {
    Versioned result;
    size_t innerSize = stream.readInt();
    result.inner = std::unordered_map<int, UnitAction>();
    result.inner.reserve(innerSize);
    for (size_t i = 0; i < innerSize; i++) {
        int innerKey;
        innerKey = stream.readInt();
        UnitAction innerValue;
        innerValue = UnitAction::readFrom(stream);
        result.inner.emplace(std::make_pair(innerKey, innerValue));
    }
    return result;
}
void Versioned::writeTo(OutputStream& stream) const {
    stream.write(43981);
    stream.write((int)(inner.size()));
    for (const auto& innerEntry : inner) {
        stream.write(innerEntry.first);
        innerEntry.second.writeTo(stream);
    }
}
std::string Versioned::toString() const {
    return std::string("Versioned") + "(" +
        "TODO" + 
        ")";
}
