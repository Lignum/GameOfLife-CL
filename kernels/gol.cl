#define INDEX(x, y) (y) * width + (x)
#define CELL(x, y) board[INDEX(x, y)]

__kernel void step_board(__global int *board, int width, int height) {
    const int x = get_global_id(0);
    const int y = get_global_id(1);

    const int left = CELL(x - 1, y);
    const int right = CELL(x + 1, y);
    const int bottom = CELL(x, y + 1);
    const int top = CELL(x, y - 1);

    const int topLeft = CELL(x - 1, y - 1);
    const int topRight = CELL(x + 1, y - 1);
    const int bottomLeft = CELL(x - 1, y + 1);
    const int bottomRight = CELL(x + 1, y + 1);

    const int count = left + right + bottom + top + topLeft + topRight + bottomLeft + bottomRight;

    const int idx = INDEX(x, y);

    const int die = count < 2 || count > 3;
    const int reproduce = count == 3;

    mem_fence(CLK_GLOBAL_MEM_FENCE);
    board[idx] = select(select(board[idx], 1, reproduce), 0, die);
}