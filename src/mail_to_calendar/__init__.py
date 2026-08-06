"""Rule-based local mail-to-calendar integration for AgentLedger."""

from .extractor import RuleBasedCalendarExtractor, to_calendar_request
from .models import CalendarCandidate, EmailMessage, ImportanceResult
from .service import MailToCalendarService, ProcessingResult

__all__ = [
    "CalendarCandidate",
    "EmailMessage",
    "ImportanceResult",
    "MailToCalendarService",
    "ProcessingResult",
    "RuleBasedCalendarExtractor",
    "to_calendar_request",
]
