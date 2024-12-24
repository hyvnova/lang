"""
Hendrick Y. Rodriguez 
Project 8 


Design a payroll program that prompts the user to enter an employee's hourly pay rate and the number of hours worked.  
Validate the user's input so that only pay rates in the range of $7.50 through $18.25 
and hours in the range of 0 through 40 are accepted.  
The program should display the employee's gross pay
"""


# This is sort of do-while loop
# This will keep asking the user for input until the user enters a valid input
# Get the hourly pay rate
while True:
    try:
        hourly_rate = float(input("Enter the hourly pay rate: "))
    except ValueError:
        print("Invalid input. Please enter a valid number")
        continue

    # Validate the input
    if 7.50 <= hourly_rate <= 18.25: break
    else: print("Invalid input. Please enter a pay rate between $7.50 and $18.25")

# Get the number of hours worked
while True:
    try:
        hours_worked = float(input("Enter the number of hours worked: "))
    except ValueError:
        print("Invalid input. Please enter a valid number")
        continue

    # Validate the input
    if 0 <= hours_worked <= 40: break
    else: print("Invalid input. Please enter a number of hours between 0 and 40")

# Calculate and print  gross pay
print(
    f"The employee's gross pay is ${hourly_rate * hours_worked:.2f}"
)                                                                        