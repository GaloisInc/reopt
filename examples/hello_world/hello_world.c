
int puts(const char *s);


void h() {
    puts("Hello World!");
}

void g() {
    h();
}

void f() {
    g();
}

int main(int argc, char** argv) {
    f();
    return 0;
}
