#include <cassert>
using namespace std;

struct RGB{
    int red;
    int green;
    int blue;
};

struct CMYK{
    float cyan;
    float megenta;
    float yello;
    float key;
};

class Colour{
public:
    Colour(int r, int g, int b) : type(Type::RGB) { value.rgb = RGB{ r, g, b }; }
    Colour(float c, float m, float y, float k) : type(Type::CMYK) {value.cmyk = CMYK{c, m, y, k};}

    enum class Type {RGB, CMYK} type;
    const RGB& rgb() const{
        assert(type == Type::RGB);
        return value.rgb;
    }

    const CMYK& cmyk() const{
        assert(type == Type::CMYK);
        return value.cmyk;
    }

private:
    union {
        RGB rgb;
        CMYK cmyk;
    } value;

    const RGB& rgb() const{
        assert(type == Type::RGB);
        return value.rgb;
    }
}
