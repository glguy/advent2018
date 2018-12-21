#include <iostream>
#include <unordered_set>

long run() {
    std::unordered_set<long> seen;
    long r1=0L, r3=0L, r4=0L, r5=0L;
    long prev = 0;

    r5 = 123; //seti 123 0 5
    line2: r5 &= 456; // bani 5 456 5
    r5 = r5 == 72; // eqri 5 72 5
    if (r5) goto line6; // addr 5 2 2
    goto line2; // seti 0 0 2
    line6: r5 = 0; // seti 0 4 5
    line7: r4 = r5 | 65536; // bori 5 65536 4
    r5 = 15466939; // seti 15466939 9 5
    line9: r3 = r4 & 255; // bani 4 255 3
    r5 += r3; // addr 5 3 5
    r5 &= 16777215; // bani 5 16777215 5
    r5 *= 65899; // muli 5 65899 5
    r5 &= 16777215; // bani 5 16777215 5
    r3 = 256 > r4; // gtir 256 4 3
    if (r3) goto line17; // addr 3 2 2
    goto line18; // addi 2 1 2
    line17: goto line29; // seti 27 8 2
    line18: r3 = 0; // seti 0 7 3
    line19: r1 = r3 + 1; // addi 3 1 1
    r1 *= 256; // muli 1 256 1
    r1 = r1 > r4; // gtrr 1 4 1
    if (r1) goto line24; // addr 1 2 2
    goto line25; // addi 2 1 2
    line24: goto line27; // seti 25 2 2
    line25: r3 += 1; // addi 3 1 3
    goto line19; // seti 17 7 2
    line27: r4 = r3; // setr 3 7 4
    goto line9; // seti 7 3 2
    line29:

    if (!seen.insert(r5).second) {
        return prev;
    }
    prev = r5;

    /*
    line29: r3 = r5 == r0; // eqrr 5 0 3
    if (r3) { return r0; } // addr 3 2 2
    */
    goto line7; // seti 5 9 2
}

int main(void) {
    std::cout << run() << std::endl;
}