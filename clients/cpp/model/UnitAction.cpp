#include "UnitAction.hpp"

UnitAction::UnitAction() { }
UnitAction::UnitAction(double velocity, bool jump, bool jumpDown, Vec2Double aim, bool shoot, bool reload, bool swapWeapon, bool plantMine) : velocity(velocity), jump(jump), jumpDown(jumpDown), aim(aim), shoot(shoot), reload(reload), swapWeapon(swapWeapon), plantMine(plantMine) { }
UnitAction UnitAction::readFrom(InputStream& stream) {
    UnitAction result;
    result.velocity = stream.readDouble();
    result.jump = stream.readBool();
    result.jumpDown = stream.readBool();
    result.aim = Vec2Double::readFrom(stream);
    result.shoot = stream.readBool();
    result.reload = stream.readBool();
    result.swapWeapon = stream.readBool();
    result.plantMine = stream.readBool();
    return result;
}
void UnitAction::writeTo(OutputStream& stream) const {
    stream.write(velocity);
    stream.write(jump);
    stream.write(jumpDown);
    aim.writeTo(stream);
    stream.write(shoot);
    stream.write(reload);
    stream.write(swapWeapon);
    stream.write(plantMine);
}
std::string UnitAction::toString() const {
    return std::string("UnitAction") + "(" +
        std::to_string(velocity) +
        (jump ? "true" : "false") + 
        (jumpDown ? "true" : "false") + 
        aim.toString() +
        (shoot ? "true" : "false") + 
        (reload ? "true" : "false") + 
        (swapWeapon ? "true" : "false") + 
        (plantMine ? "true" : "false") + 
        ")";
}
