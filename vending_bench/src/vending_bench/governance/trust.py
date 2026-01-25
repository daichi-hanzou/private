"""
Trust level system for information source verification.
Helps resist social engineering and misinformation attacks.
"""

from __future__ import annotations

from dataclasses import dataclass
from datetime import datetime
from enum import Enum
from typing import Any


class TrustLevel(Enum):
    """Trust level of an information source."""

    UNVERIFIED = "unverified"  # Unknown source, not verified
    VERIFIED_SUPPLIER = "verified_supplier"  # Known, verified supplier
    INTERNAL_POLICY = "internal_policy"  # Internal KPIs, policies


@dataclass
class InformationSource:
    """Information with associated trust level."""

    content: str
    source_type: str  # "email", "web", "pdf", "internal"
    trust_level: TrustLevel
    timestamp: datetime
    metadata: dict[str, Any] | None = None

    def to_dict(self) -> dict[str, Any]:
        """Convert to dictionary for logging."""
        return {
            "content_preview": self.content[:100] if len(self.content) > 100 else self.content,
            "source_type": self.source_type,
            "trust_level": self.trust_level.value,
            "timestamp": self.timestamp.isoformat(),
            "metadata": self.metadata or {},
        }


class TrustManager:
    """
    Manages trust levels of information sources.
    Tracks verified suppliers and assesses credibility of incoming information.
    """

    def __init__(self):
        """Initialize trust manager."""
        self.verified_suppliers: set[str] = set()
        self.verified_domains: set[str] = set()
        self.information_history: list[InformationSource] = []

    def assess_email(self, sender: str, subject: str, body: str) -> TrustLevel:
        """
        Assess trust level of an email.

        Args:
            sender: Email sender address
            subject: Email subject
            body: Email body

        Returns:
            Trust level for this email
        """
        sender_lower = sender.lower()

        # Check if verified supplier
        if sender_lower in self.verified_suppliers:
            return TrustLevel.VERIFIED_SUPPLIER

        # Check if from verified domain
        if "@" in sender_lower:
            domain = sender_lower.split("@")[1]
            if domain in self.verified_domains:
                return TrustLevel.VERIFIED_SUPPLIER

        # Default: unverified
        return TrustLevel.UNVERIFIED

    def assess_web_content(self, url: str, content: str) -> TrustLevel:
        """
        Assess trust level of web content.

        Args:
            url: Source URL
            content: Content text

        Returns:
            Trust level for this content
        """
        # Extract domain from URL
        try:
            from urllib.parse import urlparse

            parsed = urlparse(url)
            domain = parsed.netloc.lower()

            if domain in self.verified_domains:
                return TrustLevel.VERIFIED_SUPPLIER

        except Exception:
            pass

        return TrustLevel.UNVERIFIED

    def verify_supplier(self, email_or_domain: str) -> None:
        """
        Add a supplier to the verified list.

        Args:
            email_or_domain: Email address or domain to verify
        """
        email_or_domain = email_or_domain.lower()

        if "@" in email_or_domain:
            # It's an email address
            self.verified_suppliers.add(email_or_domain)

            # Also add the domain
            domain = email_or_domain.split("@")[1]
            self.verified_domains.add(domain)
        else:
            # It's a domain
            self.verified_domains.add(email_or_domain)

    def unverify_supplier(self, email_or_domain: str) -> None:
        """
        Remove a supplier from the verified list.

        Args:
            email_or_domain: Email address or domain to unverify
        """
        email_or_domain = email_or_domain.lower()

        if "@" in email_or_domain:
            self.verified_suppliers.discard(email_or_domain)
        else:
            self.verified_domains.discard(email_or_domain)

    def is_high_risk_action_on_unverified(
        self,
        action_type: str,
        trust_level: TrustLevel,
    ) -> bool:
        """
        Check if an action based on unverified information is high risk.

        Args:
            action_type: Type of action (e.g., "price_change", "free_distribution")
            trust_level: Trust level of the information source

        Returns:
            True if this is a high-risk action on unverified info
        """
        if trust_level != TrustLevel.UNVERIFIED:
            return False

        # High-risk actions
        high_risk_actions = [
            "large_price_change",  # >30% price change
            "free_distribution",  # Price = 0
            "policy_change",  # Changing business policies
            "large_order",  # Order > threshold
            "category_change",  # Adding new product categories
        ]

        return action_type in high_risk_actions

    def record_information(
        self,
        content: str,
        source_type: str,
        trust_level: TrustLevel,
        metadata: dict[str, Any] | None = None,
    ) -> InformationSource:
        """
        Record an information source for auditing.

        Args:
            content: Information content
            source_type: Type of source
            trust_level: Assessed trust level
            metadata: Additional metadata

        Returns:
            Created InformationSource object
        """
        info = InformationSource(
            content=content,
            source_type=source_type,
            trust_level=trust_level,
            timestamp=datetime.now(),
            metadata=metadata,
        )

        self.information_history.append(info)
        return info

    def get_unverified_count(self, days: int = 7) -> int:
        """
        Get count of unverified information sources in recent days.

        Args:
            days: Number of days to look back

        Returns:
            Count of unverified sources
        """
        cutoff = datetime.now()
        # Simplified - would need proper date math
        count = 0

        for info in self.information_history:
            if info.trust_level == TrustLevel.UNVERIFIED:
                count += 1

        return count

    def to_dict(self) -> dict[str, Any]:
        """Convert trust manager state to dictionary."""
        return {
            "verified_suppliers_count": len(self.verified_suppliers),
            "verified_domains_count": len(self.verified_domains),
            "information_history_count": len(self.information_history),
        }
