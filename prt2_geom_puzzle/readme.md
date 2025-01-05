# Geometric Puzzle Generation System

This project is a Prolog-based system designed to generate and solve **geometric puzzles**. The puzzles challenge users to recognize patterns among a set of compound geometric figures and identify a missing figure based on these patterns. It combines logical reasoning with visual problem-solving, offering an engaging and interactive experience.


## Features

### 1. Dynamic Figure Generation
- Generates compound geometric figures composed of basic shapes such as:
  - Squares
  - Circles
  - Triangles
  - Trapezoids
  - Rectangles
- Each figure is defined by parameters like:
  - Size
  - Border color
  - Fill color
  - Nesting relationships
- Colors and configurations are assigned randomly to ensure uniqueness.

### 2. Puzzle Creation
- Automatically generates puzzles with rows of geometric figures, where one figure is missing.
- Users must identify the missing figure by analyzing the pattern in the existing rows.
- Provides multiple answer options, ensuring that at least one option is correct while others are close but subtly different.

### 3. Interactive Gameplay
- Two gameplay modes:
  1. **Solve the puzzle manually**: Users select their answer from multiple options.
  2. **Request a solution**: The system highlights the correct answer and explains the pattern.
- Feedback provided after each puzzle:
  - Correct answers are marked with a checkmark.
  - Incorrect answers are marked with a cross, and the correct solution is displayed.

### 4. Levels of Difficulty
- Three difficulty levels:
  - **Easy**: Simple patterns with fewer parameters.
  - **Medium**: Moderate complexity with more attributes.
  - **Hard**: Advanced puzzles with intricate relationships and additional figures.
- Difficulty affects the number of parameters, figures, and answer options.

### 5. Randomized Puzzle Generation
- Uses random parameters like the current time to generate unique puzzles in every session.
- Ensures a new and challenging experience for each game.


## Technologies Used

### 1. Prolog Logic Programming
- Handles figure generation, puzzle validation, and gameplay mechanics.
- Core modules:
  - `figure_logic.pl`: Manages figure creation and puzzle logic.
  - `game_logic.pl`: Handles game progression and difficulty levels.

### 2. XPCE (Prolog GUI Toolkit)
- Provides a graphical user interface for interaction.
- Features include:
  - Puzzle visualization
  - Answer selection
  - Dynamic feedback using visual elements (checkmarks and crosses).


## How to Run the Project

1. **Clone the repository:**
   ```bash
   git clone https://github.com/your-username/geometric-puzzle-generator.git
   cd geometric-puzzle-generator
2. **Install Prolog (if not already installed):**
    ```bash
    sudo apt install swi-prolog
3. Run the project using the start.pl file:
    ```bash
    swipl start.pl