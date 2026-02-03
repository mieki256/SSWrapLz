#!python
# -*- mode: python; Encoding: utf-8; coding: utf-8 -*-
# Last updated: <2026/01/11 19:45:06 +0900>
"""
pygame fullscreen sample.

Windows11 x64 25H2 + Python 3.10.10 64bit + pygame-ce 2.5.6
"""

import ctypes
import math
import os

APP_MUTEX_NAME = "PygameScreenSaverAppMutex_12345"

DBG = False
# DBG = True


def main():
    # mutex
    kernel32 = ctypes.windll.kernel32
    mutex = kernel32.CreateMutexW(None, True, APP_MUTEX_NAME)
    result = kernel32.WaitForSingleObject(mutex, 0)
    if result != 0:
        if DBG:
            print("This application is already running.")

        # close mutex
        kernel32.ReleaseMutex(mutex)
        kernel32.CloseHandle(mutex)
        return

    # Hide pygame welcome message
    os.environ["PYGAME_HIDE_SUPPORT_PROMPT"] = "1"
    import pygame

    pygame.init()
    if DBG:
        screen = pygame.display.set_mode((1280, 720))
    else:
        screen = pygame.display.set_mode((0, 0), pygame.FULLSCREEN)

    pygame.display.set_caption("pygame Fullscreen Test")

    if not DBG:
        pygame.mouse.set_visible(False)

    clock = pygame.time.Clock()

    font0 = pygame.font.SysFont("Tahoma", 48, True)
    col = pygame.Color(64, 128, 192)
    img = font0.render("Python + pygame", True, col)

    x, y = 0, 0
    ang_x, ang_y = 0.0, 0.0
    running = True

    while running:
        for ev in pygame.event.get():
            if ev.type == pygame.QUIT:
                running = False
            elif not DBG:
                if ev.type == pygame.KEYDOWN:
                    running = False
                elif ev.type == pygame.MOUSEBUTTONDOWN:
                    running = False
                elif ev.type == pygame.MOUSEMOTION:
                    dx, dy = pygame.mouse.get_rel()
                    d = 16
                    if dx * dx + dy * dy > d * d:
                        running = False

        sw, sh = screen.get_width(), screen.get_height()
        iw, ih = img.get_width(), img.get_height()
        xr, yr = (sw / 2) - (iw / 2), (sh / 2) - (ih / 2)
        x = xr * math.cos(math.radians(ang_x)) + (sw / 2) - (iw / 2)
        y = yr * math.sin(math.radians(ang_y)) + (sh / 2) - (ih / 2)
        ang_x += 1.0
        ang_y += 0.75

        screen.fill((0, 0, 32))
        screen.blit(img, (x, y))
        pygame.display.flip()

        clock.tick_busy_loop(60)

    pygame.quit()

    # close mutex
    kernel32.ReleaseMutex(mutex)
    kernel32.CloseHandle(mutex)


if __name__ == "__main__":
    main()
