# Trip Planner in Prolog

This repository contains a Prolog program that solves the problem of finding tourist routes based on user-defined criteria. The program reads data about available tourist routes from a file and allows users to interactively search for routes that meet specific requirements.

## Problem Statement

The task is to write a Prolog program that reads information about available tourist routes from a file (specified as an argument) and then executes a sequence of commands provided by the user interactively. Each command involves finding all trips that meet the specified requirements. The output for each trip includes details about all the places the trip goes through, the route numbers, their types, and the total length of the trip.

### Input File Format

The input file contains descriptions of tourist routes, where each route is defined as a Prolog fact in the following format:

trasa(Tid, Start, End, Type, Direction, Distance).

- `Tid` — unique identifier of the route.
- `Start` — starting point of the route.
- `End` — ending point of the route.
- `Type` — type of route (e.g., rower, piesza, gorska).
- `Direction` — indicates if the route is available in both directions (`oba`) or only from start to end (`jeden`).
- `Distance` — length of the route in kilometers.

### Example Data File: `tatry.txt`

The file `tatry.txt` contains the following routes:

trasa(r1, zakopane, brzeziny, rower, oba, 25).  
trasa(r2, brzeziny, gasienicowa, rower, oba, 15).  
trasa(r3, brzeziny, poroniec, rower, oba, 10).  
trasa(r4, poroniec, rusinowa, rower, oba, 6).  
trasa(g1, zakopane, kuznice, spacer, oba, 7).  
trasa(g2, zakopane, kalatowki, gorska, oba, 5).  
trasa(g3, kuznice, gasienicowa, gorska, oba, 7).  
trasa(g4, gasienicowa, zawrat, gorska, oba, 6).  
trasa(g5, gasienicowa, czarnystaw, gorska, oba, 3).  
trasa(g6, zawrat, kozia, gorska, jeden, 5).  
trasa(g7, kozia, gasienicowa, gorska, jeden, 7).  
trasa(p1, zakopane, gubalowka, piesza, oba, 5).

## How the Program Works

1. **Reading the File**: The program begins by reading the file containing route data. The filename is provided as an argument when starting the Prolog program.

2. **Interactive User Input**: The program operates in an interactive mode, asking the user for:
   - The starting point of the trip.
   - The ending point of the trip.
   - The conditions that the trip should meet (such as route type or distance).

3. **Generating Trips**: Based on the user input, the program generates all possible trips that meet the specified conditions, ensuring no route is used more than once unless it's allowed to be traversed in both directions.

4. **Output**: For each valid trip, the program outputs the sequence of routes, their types, and the total distance.

### Key Parts of the Code

- **Reading and Parsing Input**: 
  - The function `read_conditions/1` is responsible for reading the user input related to trip conditions. It uses the `read/1` predicate to capture input from the user, which can include the type of route (`rodzaj/1`) and the length conditions (`dlugosc/2`). The input is then converted into a list of conditions using the helper function `convert_input_tuples_to_list/2`. This function recursively processes the input tuple, converting it into a list format that is easier to work with during trip generation.

- **Generating Trips**: 
  - The predicate `generate_trip_meeting_conditions/5` initializes the trip generation process. It sets up the initial conditions with an empty list for visited routes and a length accumulator starting at 0. This predicate then calls `generate_trip/7`, which is the core recursive predicate responsible for exploring all possible routes from the starting point to the endpoint. 
  - `generate_trip/7` works by selecting a route that starts from the current location and checks if it matches the user-defined conditions. It ensures that routes are not revisited unless they can be traversed in both directions (`oba`). If a valid route is found, the predicate recursively continues to the next possible location, accumulating the total distance and tracking visited routes. This process repeats until the endpoint is reached or no more valid routes are available.

- **Checking Conditions**: 
  - The predicates `route_meets_type_conditions/2` and `check_length/2` are crucial for validating that each segment of a trip meets the user-defined criteria. 
  - `route_meets_type_conditions/2` checks if a specific route (`Tid`) matches any of the types specified in the conditions. It does this by retrieving the type of the route from the route database and comparing it against the allowed types using the `check_type/2` predicate.
  - `check_length/2` ensures that the total length of the trip satisfies the length condition provided by the user. This predicate examines the conditions list for any length constraints and compares the accumulated trip length against these constraints using the helper predicates `check_operator/3`. These predicates handle different comparison operators (`eq`, `lt`, `le`, `gt`, `ge`) to accurately enforce the user-defined limits.

- **Printing Results**: 
  - The predicates `print_single_trip/3` and `print_all_trips/4` handle the formatting and output of trips that satisfy the conditions.
  - `print_single_trip/3` prints each segment of a trip, detailing the start and end points, the route identifier (`Tid`), and the type of the route. It handles both directions of routes (`jeden` and `oba`), ensuring that the output accurately reflects the route taken.
  - `print_all_trips/4` iterates over all valid trips generated by the program. For each trip, it calls `print_single_trip/3` to output the details and then prints the total length of the trip. This provides a clear, formatted output of all possible routes that meet the user's criteria, making it easy for the user to understand the options available.

By breaking down the code into these key parts, the program ensures a modular approach, making it easier to maintain and extend. Each part of the code is dedicated to a specific task, which promotes clarity and separation of concerns in the program’s design. This approach not only helps in achieving the desired functionality but also makes the codebase more readable and easier to debug.


## Example Usage

To run the program using the `tatry.txt` data file, follow these steps:

1. Start the Prolog interpreter and load your solution file:

    ```
    sicstus -l trip_planner.pl
    ```

2. Execute the program by specifying the data file as an argument:

    ```
    ?- user:runtime_entry(start), consult('trip_planner'), wyprawy('tatry.txt').
    ```

3. Follow the interactive prompts to enter the starting point, ending point, and conditions.

   For example:
   - **Starting point**: `zakopane.`
   - **Ending point**: `rusinowa.`
   - **Conditions**: `rodzaj(rower).`

   The program will then display all possible routes from Zakopane to Rusinowa that are of the type "rower" (bike).

## Conclusion

This Prolog program provides a flexible way to find tourist routes based on user preferences, utilizing interactive input and comprehensive checks to ensure all conditions are met.


