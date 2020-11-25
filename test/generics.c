int main(int argc, char *argv[]) {
    int x = _Generic('a', char: 1, default: 0);
    return x;
}