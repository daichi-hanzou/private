"""
Account (money balance) management for Vending-Bench.
Tracks the agent's cash on hand and transaction history.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from datetime import date
from enum import Enum
from typing import Any


class TransactionType(Enum):
    """Types of financial transactions."""

    INITIAL_BALANCE = "initial_balance"
    DAILY_FEE = "daily_fee"
    PURCHASE = "purchase"  # Buying from supplier
    CASH_COLLECTION = "cash_collection"  # Collecting from vending machine
    REFUND = "refund"


@dataclass
class Transaction:
    """A single financial transaction."""

    type: TransactionType
    amount: float  # Positive = credit, Negative = debit
    balance_after: float
    description: str
    date: date
    day_number: int

    def to_dict(self) -> dict[str, Any]:
        return {
            "type": self.type.value,
            "amount": self.amount,
            "balance_after": self.balance_after,
            "description": self.description,
            "date": self.date.isoformat(),
            "day_number": self.day_number,
        }


@dataclass
class Account:
    """
    Financial account for the vending machine business.

    Tracks:
    - Current balance (cash on hand)
    - Transaction history
    - Consecutive days unable to pay daily fee (for bankruptcy check)
    """

    balance: float = 0.0
    transactions: list[Transaction] = field(default_factory=list)
    consecutive_fee_failures: int = 0
    account_number: str = "VB-001234567"  # For supplier billing

    @classmethod
    def create(cls, initial_balance: float) -> Account:
        """Create a new account with initial balance."""
        account = cls(balance=initial_balance)
        # Record initial balance as first transaction
        account.transactions.append(
            Transaction(
                type=TransactionType.INITIAL_BALANCE,
                amount=initial_balance,
                balance_after=initial_balance,
                description="Initial balance",
                date=date.today(),
                day_number=1,
            )
        )
        return account

    def debit(
        self,
        amount: float,
        transaction_type: TransactionType,
        description: str,
        current_date: date,
        day_number: int,
    ) -> bool:
        """
        Debit (subtract) money from the account.
        Returns True if successful, False if insufficient funds.
        """
        if amount <= 0:
            return False

        if self.balance < amount:
            return False

        self.balance -= amount
        self.transactions.append(
            Transaction(
                type=transaction_type,
                amount=-amount,
                balance_after=self.balance,
                description=description,
                date=current_date,
                day_number=day_number,
            )
        )
        return True

    def credit(
        self,
        amount: float,
        transaction_type: TransactionType,
        description: str,
        current_date: date,
        day_number: int,
    ) -> None:
        """Credit (add) money to the account."""
        if amount <= 0:
            return

        self.balance += amount
        self.transactions.append(
            Transaction(
                type=transaction_type,
                amount=amount,
                balance_after=self.balance,
                description=description,
                date=current_date,
                day_number=day_number,
            )
        )

    def pay_daily_fee(
        self,
        fee: float,
        current_date: date,
        day_number: int,
    ) -> bool:
        """
        Attempt to pay the daily operating fee.
        Returns True if successful, False if insufficient funds.
        Updates consecutive failure counter.
        """
        success = self.debit(
            amount=fee,
            transaction_type=TransactionType.DAILY_FEE,
            description=f"Daily operating fee for day {day_number}",
            current_date=current_date,
            day_number=day_number,
        )

        if success:
            self.consecutive_fee_failures = 0
        else:
            self.consecutive_fee_failures += 1

        return success

    def make_purchase(
        self,
        amount: float,
        supplier: str,
        items: str,
        current_date: date,
        day_number: int,
    ) -> bool:
        """
        Make a purchase from a supplier.
        Returns True if successful, False if insufficient funds.
        """
        return self.debit(
            amount=amount,
            transaction_type=TransactionType.PURCHASE,
            description=f"Purchase from {supplier}: {items}",
            current_date=current_date,
            day_number=day_number,
        )

    def collect_cash(
        self,
        amount: float,
        current_date: date,
        day_number: int,
    ) -> None:
        """Credit cash collected from the vending machine."""
        self.credit(
            amount=amount,
            transaction_type=TransactionType.CASH_COLLECTION,
            description=f"Cash collected from vending machine",
            current_date=current_date,
            day_number=day_number,
        )

    def can_afford(self, amount: float) -> bool:
        """Check if account has sufficient funds."""
        return self.balance >= amount

    def is_bankrupt(self, threshold: int = 10) -> bool:
        """
        Check if business is bankrupt.
        Bankruptcy occurs after failing to pay daily fee for `threshold` consecutive days.
        """
        return self.consecutive_fee_failures >= threshold

    def get_recent_transactions(self, count: int = 10) -> list[Transaction]:
        """Get the most recent transactions."""
        return self.transactions[-count:]

    def to_dict(self) -> dict[str, Any]:
        """Convert account state to dictionary."""
        return {
            "balance": self.balance,
            "account_number": self.account_number,
            "consecutive_fee_failures": self.consecutive_fee_failures,
            "transaction_count": len(self.transactions),
        }

    def __repr__(self) -> str:
        return f"Account(balance=${self.balance:.2f}, transactions={len(self.transactions)})"
