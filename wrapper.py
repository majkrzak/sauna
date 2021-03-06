#!/usr/bin/env python3
from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from time import sleep
from subprocess import Popen, PIPE

driver = webdriver.Firefox()
driver.get("https://sanuli.fi/")
sleep(1)


while True:
    try:
        with Popen(
            ["/usr/bin/env", "stack", "exec", "sauna-solver"], stdout=PIPE, stdin=PIPE
        ) as proc:
            line = 1
            while True:
                guess = proc.stdout.readline().decode().rstrip()
                elem = driver.find_element_by_tag_name("body")
                for key in guess:
                    elem.send_keys(key)
                    sleep(0.05)
                elem.send_keys(Keys.ENTER)
                sleep(0.2)
                result = "".join(
                    [
                        {
                            "tile present": "Y",
                            "tile absent": "B",
                            "tile correct": "G",
                        }[x.get_attribute("class")]
                        for x in driver.find_elements_by_xpath(
                            f"//div[@class='board-6']/div[{line}]/div"
                        )
                    ]
                )
                proc.stdin.write(result.encode() + b"\n")
                proc.stdin.flush()
                if result == "GGGGG":
                    line = 1
                    sleep(1)
                    elem.send_keys(Keys.ENTER)
                else:
                    line += 1
    except:
        pass


driver.close()
