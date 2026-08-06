"""Local end-to-end orchestration from email to calendar proposal."""

from .models import OrchestrationResult
from .service import MailCalendarOrchestrator

__all__ = ["MailCalendarOrchestrator", "OrchestrationResult"]
