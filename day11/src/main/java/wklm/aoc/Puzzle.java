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

class PlutonianPebbles {
    static class Stone {
        private static int digitCount(long i) {
            int count = 0;
            do {
                count++;
                i /= 10;
            } while (i > 0);
            return count;
        }

        static long[] transform(long stone) {
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
        return Arrays.stream(input.split(" "))
                .filter(s -> !s.isBlank())
                .map(Long::parseLong)
                .collect(Collectors.groupingBy(
                        Function.identity(),
                        Collectors.counting()));
    }

    static Map<Long, Long> blink(Map<Long, Long> stones) {
        Map<Long, Long> newStones = new HashMap<>();
        for (var entry : stones.entrySet()) {
            for (long s : Stone.transform(entry.getKey())) {
                newStones.merge(s, entry.getValue(), Long::sum);
            }
        }
        return newStones;
    }
}

record Puzzle(String input) {
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