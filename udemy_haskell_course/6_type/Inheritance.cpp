class BaseClass {
public:
    virtual void method() = 0;
};

class DerivedClass : public BaseClass {
public:
    void method() overide { }
};

void f0(BaseClass& obj) {
    obj.method();
}

void f1() {
    DerivedClass obj;
    f0(obj);
}
