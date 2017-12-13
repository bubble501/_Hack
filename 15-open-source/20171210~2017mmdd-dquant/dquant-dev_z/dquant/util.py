import re
import time
from dateutil.parser import parse


class Util():
    @staticmethod
    def slice_till_dot(str):
        """
        :type string
        """
        slice_till_dot = re.compile(r"^(.*?)\..*")
        match_result = re.match(slice_till_dot, str)
        if match_result is not None:
            return match_result.group(1)
        else:
            return None

    @staticmethod
    def utc_timestamp_to_time(str):
        t = parse(str)
        return time.mktime(t.timetuple()) + t.microsecond / 1E6
