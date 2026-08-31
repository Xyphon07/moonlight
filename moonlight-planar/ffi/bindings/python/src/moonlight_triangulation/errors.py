from __future__ import annotations

from ._native_generated import _Obstruction


class MoonlightError(RuntimeError):
    def __init__(self, status: int, obstruction: _Obstruction) -> None:
        self.status = status
        self.code = obstruction.code
        self.coordinate_error = obstruction.coordinate_error
        self.input_index = obstruction.input_index
        self.first_index = obstruction.first_index
        self.second_index = obstruction.second_index
        self.first_value = obstruction.first_value
        self.second_value = obstruction.second_value
        self.point = (obstruction.point_x, obstruction.point_y)
        message = bytes(obstruction.message).split(b"\0", 1)[0].decode("utf-8", errors="replace")
        super().__init__(message or f"Moonlight ABI failure {status}:{self.code}")
