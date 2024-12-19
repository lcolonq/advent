record Check(long total, int start) {}

class State {
    public int pc;
    public long regA, regB, regC;
    public String toString() {
        return String.format("{pc: %d, A: %d, B: %d, C: %d}", pc, regA, regB, regC);
    }
    public List<Long> parse(String inp) {
        var ret = new ArrayList<Long>();
        var sp = inp.split("\n\n");
        var regsp = sp[0].split("\n");
        regA = Long.parseLong(regsp[0].split(": ")[1]);
        regB = Long.parseLong(regsp[1].split(": ")[1]);
        regC = Long.parseLong(regsp[2].split(": ")[1]);
        Arrays.stream(sp[1].split(": ")[1].split(","))
            .map(x -> Long.parseLong(x.trim()))
            .forEach(x -> ret.add(x));
        return ret;
    }
    long combo(long x) {
        switch ((int) x) {
        case 0: return 0; case 1: return 1; case 2: return 2; case 3: return 3;
        case 4: return regA; case 5: return regB; case 6: return regC;
        }
        return -1;
    }
    public boolean step(List<Long> prog, List<Long> out) {
        if (pc + 1 >= prog.size()) return false;
        var opcode = prog.get(pc++);
        var operand = prog.get(pc++);
        switch ((int) (long) opcode) {
        case 0: regA >>= combo(operand); break; // adv
        case 1: regB ^= operand; break; // bxl
        case 2: regB = combo(operand) % 8; break; // bst
        case 3: pc = regA == 0 ? pc : (int) (long) operand; break; // jnz
        case 4: regB ^= regC; break; // bxc
        case 5: out.add(combo(operand) % 8); break; // out
        case 6: regB = regA >> combo(operand); break; // bdv
        case 7: regC = regA >> combo(operand); break; // cdv
        default: System.out.println(String.format("unknown opcode: %d", opcode)); break;
        }
        return true;
    }
    long findA(List<Long> prog) {
        var out = new ArrayList<Long>();
        var checks = new ArrayDeque<Check>();
        for (int i = 0; i < 8; ++i) checks.push(new Check(i, prog.size() - 1));
        while (!checks.isEmpty() && checks.pop() instanceof Check(var total, var s)) {
            pc = 0; regA = total; regB = 0; regC = 0;
            out.clear();
            while (step(prog, out));
            boolean m = true;
            if (prog.size() - s > out.size()) m = false;
            else for (int i = 0; s + i < prog.size(); ++i) if (out.get(i) != prog.get(s + i)) m = false;
            if (m) {
                if (s == 0) return total;
                for (int i = 0; i < 8; ++i) checks.push(new Check((total << 3) | i, s - 1));
            }
        }
        return -1;
    }
}

void main(String[] args) throws IOException {
    State cpu = new State();
    String inp = Files.readString(Paths.get("input.txt"), Charset.defaultCharset());
    var prog = cpu.parse(inp);
    System.out.println("initial: " + cpu.toString());
    var out = new ArrayList<Long>();
    while (cpu.step(prog, out));
    System.out.println("final:   " + cpu.toString());
    System.out.println(String.join(",", out.stream().map(x -> x.toString()).collect(Collectors.toList())));
    var a = cpu.findA(prog);
    System.out.println(String.format("a: %d", a));
}
