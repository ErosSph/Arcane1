import os
import subprocess
import time
from concurrent.futures import ThreadPoolExecutor, as_completed
from typing import Optional

def check_single_sva(i: int, sva: str, prj_path: str, orig_property_sva: str) -> tuple[int, int]:
    new_property_path = os.path.join(prj_path, f"property_{i}.sva")

    with open(orig_property_sva, "r", encoding="utf-8") as f:
        lines = f.readlines()

    for line_num in range(len(lines)-1, -1, -1):
        if lines[line_num].strip() == "endmodule":
            endmodule_idx = line_num
            break
    else:
        endmodule_idx = len(lines)

    new_lines = lines[:endmodule_idx] + [sva + '\n'] + lines[endmodule_idx:]

    with open(new_property_path, "w", encoding="utf-8") as f:
        f.writelines(new_lines)

    completed_process = subprocess.run(
        ["/mnt/sdb/workspace/smy/dac2026/svlint/bin/svlint", new_property_path],
        capture_output=True,
        text=True,
        encoding="utf-8"
    )
    return_code = completed_process.returncode

    if "false" in sva:
        return_code = 1

    if os.path.exists(new_property_path):
        os.remove(new_property_path)

    return (i, return_code)


def check_syntax(svas: list[str], prj_path: str, max_workers: Optional[int] = None) -> list[int]:
    orig_property_sva = os.path.join(prj_path, "property.sva")
    result = [0] * len(svas)

    with ThreadPoolExecutor(max_workers=max_workers) as executor:
        future_to_index = {
            executor.submit(check_single_sva, i, sva, prj_path, orig_property_sva): i
            for i, sva in enumerate(svas)
        }

        for future in as_completed(future_to_index):
            try:
                index, return_code = future.result()
                result[index] = return_code
            except Exception:
                original_index = future_to_index[future]
                result[original_index] = -1

    return result

def generate_toml_file(prj_path: str):
    toml_path = os.path.join(prj_path, ".svlint.toml")
    with open(toml_path, "w", encoding="utf-8") as f:
        pass

example = [
    'assert property(@(posedge DEFAULT_CLOCK) ((res >= 1964426986) && (res <= 2607366454)) |-> (c >= 0) && (c <= 4005366493));',
    'assert property(@(posedge DEFAULT_CLOCK) ((c >= 1964426986) && (c <= 2607366454)) |-> (c >= 0) && (c <= 4005366493));',
]

if __name__ == "__main__":
    start_time = time.time()
    generate_toml_file("/mnt/sdb/workspace/smy/dac2026/reward")
    result = check_syntax(example, "./reward", max_workers=32)
    end_time = time.time()
