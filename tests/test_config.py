import pytest

from circular_coffee.config import build_default_config


def test_build_default_config_rejects_unknown_field() -> None:
    with pytest.raises(ValueError, match="unknown config field"):
        build_default_config(does_not_exist=123)
