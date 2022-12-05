# Project Proposal

We will build a clone of [Splitwise](https://www.splitwise.com/), an application to split shared expenses with friends. It will be a command-line program interacting with users through the terminal with core features such as data persistence and algorithms to minimize the number of transactions required to settle the debts. We will utilize the brick library to implement a user-friendly graphical interface to handle billing input and display suggested transactions. The implementation will be broken down into 4 parts. They are input handling and internal data representation, algorithms to find the minimal number of transactions, database interaction, and transaction display. We will assign each implementation task to each of our group members and collaborate closely.

## Input handling and internal data representation
The basic input for one record needs four elements: title, creditor(s), debtor(s), and amount. Creditors are people who pay for it, debtors are people you pay for(most of the cases should be you and others). For example, if A pays 100 for A and B, then the input would be creditors: A and debtors: A, B (split by comma, no comma at the end).

After the input,  we use a map to make reflections from name to amount. Its key would be the names of people, and its value includes the amount(positive or negative two decimal float), from/to whom.

## Algorithms to find the minimal number of transactions
We will refer to the algorithm implemented in Splitwise ([Splitwise is NP-Complete](https://www.alexirpan.com/2016/05/10/may-10.html)) to find the minimal number of transactions.

## Database interaction
We will use SQLite as our storage layer and the sqlite-simple library ([sqlite-simple: Mid-Level SQLite client library](https://github.com/nurpax/sqlite-simple)) for interacting with the database. We will persist user billing data into the database so that users can access their data anytime.

The model will be defined as:
```
Expense record
{
Billing_ID: int
Title: string
Creditor: [string ...]
Debtor:[string ...]
Amount: float
} 
```
| Field      | Type |
| ----------- | ----------- |
| Billing_ID | Int |
| Title | String |
| Creditor | [string ...] |
| Debtor | [string ...] |
| Amount | Float |

<!-- Billing_ID
Int
Title
String
Creditor
[string …]
Debtor
[string …]
Amount
Float -->

```
Split suggestion
{
Debtor: String
CreditorA: float
CreditorB: float
CreditorC: float
...
} 
```

| Field      | Type |
| ----------- | ----------- |
| Debtor | String |
| CreditorA | Float |

<!-- Debtor
String
CreditorA
Float -->


## Transaction display
We will use the [Brick](https://github.com/jtdaugherty/brick/) Library to a build terminal user interface (TUI) as the main interface for Splitwise, which will simplify the engineering difficulty and bring a good user experience. 

On the TUI, each expense incurred will be clearly displayed, and the split suggestions between shared users given by algorithms will also be kept up-to-date with the expense records. Therefore, the interface will be designed into three parts: the expense records, the split suggestions, and the user’s input field.

We can abstract this part of the program into a view, which are representations of the user's input and the processed content in the database.


## Setup
To run the program:
```
$stack build
$stack run
```

To run the unit tests:
```
$stack test
```