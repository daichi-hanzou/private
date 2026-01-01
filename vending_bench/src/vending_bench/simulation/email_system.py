"""
Email system for Vending-Bench.
Handles inbox/outbox for agent communication with suppliers.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any


class EmailStatus(Enum):
    """Status of an email."""

    UNREAD = "unread"
    READ = "read"


@dataclass
class Email:
    """An email message."""

    id: str
    sender: str
    recipient: str
    subject: str
    body: str
    timestamp: datetime
    day_number: int
    status: EmailStatus = EmailStatus.UNREAD
    reply_to: str | None = None  # ID of email this is replying to

    def mark_read(self) -> None:
        """Mark email as read."""
        self.status = EmailStatus.READ

    def to_dict(self) -> dict[str, Any]:
        return {
            "id": self.id,
            "sender": self.sender,
            "recipient": self.recipient,
            "subject": self.subject,
            "body": self.body,
            "timestamp": self.timestamp.isoformat(),
            "day_number": self.day_number,
            "status": self.status.value,
            "reply_to": self.reply_to,
        }

    def format_for_display(self, include_body: bool = True) -> str:
        """Format email for display to agent."""
        lines = [
            f"From: {self.sender}",
            f"To: {self.recipient}",
            f"Subject: {self.subject}",
            f"Date: {self.timestamp.strftime('%Y-%m-%d %H:%M')}",
            f"Status: {self.status.value}",
        ]
        if include_body:
            lines.append("")
            lines.append(self.body)
        return "\n".join(lines)


@dataclass
class EmailSystem:
    """
    Email system managing inbox and outbox.

    Handles:
    - Receiving emails from suppliers
    - Sending emails to suppliers
    - Tracking read/unread status
    """

    inbox: list[Email] = field(default_factory=list)
    outbox: list[Email] = field(default_factory=list)
    _next_id: int = 0

    # Agent's email address (used as sender for outgoing)
    agent_email: str = "agent@vendingbench.local"
    agent_name: str = "John Johnson"  # Name used in paper examples

    # Physical address for deliveries
    delivery_address: str = "123 Vending Street, Business District, CA 90210"

    def _generate_id(self) -> str:
        """Generate unique email ID."""
        self._next_id += 1
        return f"email_{self._next_id}"

    def send_email(
        self,
        recipient: str,
        subject: str,
        body: str,
        timestamp: datetime,
        day_number: int,
    ) -> Email:
        """
        Send an email (add to outbox).

        Returns the sent email.
        """
        email = Email(
            id=self._generate_id(),
            sender=self.agent_email,
            recipient=recipient,
            subject=subject,
            body=body,
            timestamp=timestamp,
            day_number=day_number,
            status=EmailStatus.READ,  # Outgoing emails are "read"
        )
        self.outbox.append(email)
        return email

    def receive_email(
        self,
        sender: str,
        subject: str,
        body: str,
        timestamp: datetime,
        day_number: int,
        reply_to: str | None = None,
    ) -> Email:
        """
        Receive an email (add to inbox).

        Returns the received email.
        """
        email = Email(
            id=self._generate_id(),
            sender=sender,
            recipient=self.agent_email,
            subject=subject,
            body=body,
            timestamp=timestamp,
            day_number=day_number,
            status=EmailStatus.UNREAD,
            reply_to=reply_to,
        )
        self.inbox.append(email)
        return email

    def get_inbox_summary(self, unread_only: bool = False) -> list[dict[str, Any]]:
        """Get summary of inbox emails."""
        emails = self.inbox
        if unread_only:
            emails = [e for e in emails if e.status == EmailStatus.UNREAD]

        return [
            {
                "id": e.id,
                "from": e.sender,
                "subject": e.subject,
                "date": e.timestamp.strftime("%Y-%m-%d %H:%M"),
                "status": e.status.value,
            }
            for e in reversed(emails)  # Most recent first
        ]

    def get_email_by_id(self, email_id: str) -> Email | None:
        """Get a specific email by ID."""
        for email in self.inbox + self.outbox:
            if email.id == email_id:
                return email
        return None

    def read_email(self, email_id: str) -> Email | None:
        """Read an email (marks as read)."""
        email = self.get_email_by_id(email_id)
        if email:
            email.mark_read()
        return email

    def get_unread_count(self) -> int:
        """Get count of unread emails."""
        return sum(1 for e in self.inbox if e.status == EmailStatus.UNREAD)

    def get_new_emails_since_day(self, day_number: int) -> list[Email]:
        """Get emails received since a specific day (exclusive)."""
        return [e for e in self.inbox if e.day_number > day_number]

    def get_sent_to(self, recipient: str) -> list[Email]:
        """Get all emails sent to a specific recipient."""
        return [e for e in self.outbox if e.recipient == recipient]

    def format_inbox(self, limit: int = 20) -> str:
        """Format inbox for display."""
        if not self.inbox:
            return "[Inbox is empty]"

        lines = [f"Inbox ({len(self.inbox)} emails, {self.get_unread_count()} unread):"]
        lines.append("-" * 60)

        for email in reversed(self.inbox[-limit:]):
            status_marker = "*" if email.status == EmailStatus.UNREAD else " "
            lines.append(
                f"{status_marker} [{email.id}] {email.timestamp.strftime('%m-%d %H:%M')} "
                f"From: {email.sender[:30]}"
            )
            lines.append(f"    Subject: {email.subject[:50]}")

        if len(self.inbox) > limit:
            lines.append(f"  ... and {len(self.inbox) - limit} more emails")

        return "\n".join(lines)

    def to_dict(self) -> dict[str, Any]:
        """Convert email system to dictionary."""
        return {
            "inbox_count": len(self.inbox),
            "outbox_count": len(self.outbox),
            "unread_count": self.get_unread_count(),
            "agent_email": self.agent_email,
            "delivery_address": self.delivery_address,
        }

    def __repr__(self) -> str:
        return f"EmailSystem(inbox={len(self.inbox)}, outbox={len(self.outbox)})"
