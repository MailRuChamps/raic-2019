#ifndef _MODEL_JUMP_STATE_HPP_
#define _MODEL_JUMP_STATE_HPP_

#include "../Stream.hpp"
#include <string>

class JumpState {
public:
    bool canJump;
    double speed;
    double maxTime;
    bool canCancel;
    JumpState();
    JumpState(bool canJump, double speed, double maxTime, bool canCancel);
    static JumpState readFrom(InputStream& stream);
    void writeTo(OutputStream& stream) const;
    std::string toString() const;
};

#endif
