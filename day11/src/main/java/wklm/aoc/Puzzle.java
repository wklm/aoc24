package wklm.aoc;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

final class PlutonianPebbles {
    final static class Stone {
        private static final int digitCount(long i) {
            return (int) Math.log10(i) + 1; 
        }

        static final long[] transform(long stone) {
            if (stone == 0) {
                return new long[]{1};
            }
            int digitCount = digitCount(stone);
            if (digitCount % 2 == 0) {
                long divisor = (long) Math.pow(10, digitCount / 2);
                return new long[]{stone / divisor, stone % divisor};
            }
            return new long[]{stone * 2024L};
        }
    }

    static Map<Long, Long> getStones(String input) {
        return Arrays.stream(input.split("\\s+"))
                .map(Long::parseLong)
                .collect(Collectors.groupingBy(
                        Function.identity(),
                        Collectors.counting()));
    }

    static Map<Long, Long> blink(Map<Long, Long> stones) {
        Map<Long, Long> newStones = new HashMap<>();
        stones.forEach((stone, count) -> {
            for (long transformed : Stone.transform(stone)) {
                newStones.merge(transformed, count, Long::sum);
            }
        });
        return newStones;
    }
}

final record Puzzle(String input) {
    static String getInput() throws IOException {
        Path path = Paths.get("input.txt");
        return Files.readString(path);
    }

    public static void main(String[] args) {
        try {
            var stones = PlutonianPebbles.getStones(getInput());

            // part 1
            int blink_times = 25;
            for (int i = 0; i < blink_times; i++) {
                stones = PlutonianPebbles.blink(stones);
            }
            long totalStones = stones
                    .values()
                    .stream()
                    .mapToLong(Long::longValue)
                    .sum();
            System.out.println(totalStones);
            
            // part 2
            blink_times = 50;
            for (int i = 0; i < blink_times; i++) {
                stones = PlutonianPebbles.blink(stones);
            }
            totalStones = stones
                    .values()
                    .stream()
                    .mapToLong(Long::longValue)
                    .sum();
            System.out.println(totalStones);

        } catch (IOException e) {
            System.err.println(e.getMessage());
            System.exit(1);
        }
    }
}