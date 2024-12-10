package day10;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import org.checkerframework.checker.units.qual.t;

class HikingMap {
    Grid grid;

    static record Point(int x, int y, int value) {
    }

    static class NaryTree {
        final protected Point point;
        final List<NaryTree> children;

        NaryTree(Point point, List<NaryTree> children) {
            this.point = point;
            this.children = Collections.unmodifiableList(new LinkedList<>(children));
        }

        List<NaryTree> getLeaves() {
            if (children.isEmpty()) {
                return List.of(this);
            } else {
                var leaves = new LinkedList<NaryTree>();
                for (var child : children) {
                    leaves.addAll(child.getLeaves());
                }
                return leaves;
            }
        }

        @Override
        public boolean equals(Object obj) {
            if (obj == this) {
                return true;
            }
            if (!(obj instanceof NaryTree)) {
                return false;
            }
            NaryTree other = (NaryTree) obj;
            return point.equals(other.point);
        }

        @Override
        public int hashCode() {
            return Objects.hash(point);
        }
    }

    static class Grid {
        private final Point[][] rows;
        private final Map<Integer, List<Point>> locations = new HashMap<>();

        Grid(String input) {
            String[] lines = input.split("\n");
            rows = new Point[lines.length][];

            for (int i = 0; i < lines.length; i++) {
                String line = lines[i];
                char[] numbers = line.toCharArray();
                Point[] row = new Point[numbers.length];
                for (int j = 0; j < numbers.length; j++) {
                    int val = Character.getNumericValue(numbers[j]);
                    Point p = new Point(i, j, val);
                    row[j] = p;
                    locations.computeIfAbsent(val, k -> new LinkedList<>()).add(p);
                }
                rows[i] = row;
            }
        }

        Point get(int x, int y) {
            if (x < 0 || x >= rows.length || y < 0 || y >= rows[0].length) {
                throw new IndexOutOfBoundsException();
            }
            return rows[x][y];
        }

        int[] dim() {
            return new int[] { rows.length, rows[0].length };
        }

        List<Point> find(int value) {
            return locations.getOrDefault(value, Collections.emptyList());
        }

        List<Point> getNeighbors(Point point) {
            List<Point> neighbors = new LinkedList<>();
            int x = point.x();
            int y = point.y();
            int[] dimensions = dim();
            int nRows = dimensions[0];
            int nCols = dimensions[1];

            if (x - 1 >= 0)
                neighbors.add(get(x - 1, y));
            if (x + 1 < nRows)
                neighbors.add(get(x + 1, y));
            if (y - 1 >= 0)
                neighbors.add(get(x, y - 1));
            if (y + 1 < nCols)
                neighbors.add(get(x, y + 1));

            return neighbors;
        }
    }

    public HikingMap(String input) {
        grid = new Grid(input);
    }

    List<NaryTree> getTrailheads() {
        var zeros = grid.find(0);
        return zeros
                .stream()
                .map(this::getPaths)
                .collect(
                        Collectors.collectingAndThen(
                                Collectors.toList(),
                                Collections::unmodifiableList));
    }

    NaryTree getPaths(Point point) {
        var nextSteps = grid
                .getNeighbors(point)
                .stream()
                .filter(
                        n -> grid.get(n.x, n.y).value() == point.value() + 1)
                .collect(Collectors.toList());

        List<NaryTree> paths = nextSteps.stream()
                .map(this::getPaths)
                .collect(Collectors.toList());

        return new NaryTree(grid.get(point.x, point.y), paths);
    }

    int getScore(boolean distinct) {
        return getTrailheads().stream()
                .mapToInt(trailhead -> {
                    var peaks = trailhead
                            .getLeaves()
                            .stream()
                            .filter(l -> l.point.value() == 9);
                    return distinct
                            ? (int) peaks.distinct().count()
                            : (int) peaks.count();
                })
                .sum();
    }
}

record Puzzle(String input) {
    static String getInput() throws IOException {
        Path path = Paths.get("input.txt");
        return Files.readString(path);
    }

    public static void main(String[] args) {
        try {
            var map = new HikingMap(Puzzle.getInput());
            // part 1
            System.out.println(map.getScore(true));
            // part 2
            System.out.println(map.getScore(false));
        } catch (IOException e) {
            System.err.println(e.getMessage());
            System.exit(1);
        }
    }
}
